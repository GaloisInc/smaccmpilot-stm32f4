{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module SMACCMPilot.Flight.Sensors.LIDARLite where

import Prelude ()
import Prelude.Compat

import Ivory.BSP.STM32.Driver.I2C
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.Config
import Ivory.Tower.HAL.Bus.Interface
import SMACCMPilot.Comm.Ivory.Types.LidarliteSample
import SMACCMPilot.Time
import SMACCMPilot.Hardware.Platforms.ParserUtils

data LIDARLite = LIDARLite {
    lidarlite_i2c_addr :: I2CDeviceAddr
  , lidarlite_cal_offset_cm :: Sint16
  }

parseLidar :: ConfigParser LIDARLite
parseLidar = subsection "lidarlite" $ do
  lidarlite_i2c_addr <- i2cAddr
  lidarlite_cal_offset_cm <- fromIntegral <$> subsection "offset_cm" integer
  return $ LIDARLite {..}

newtype LIDARLiteDriverState = LIDARLiteDriverState Uint8
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit, IvoryZeroVal)

llInitializing, llInactive, llInitialReq, llActive :: LIDARLiteDriverState
[llInitializing, llInactive, llInitialReq, llActive]
  = map (LIDARLiteDriverState . fromInteger) [0..3]

-- | This is dangerous! If the driver tries to send another request
-- while one is already pending from the scheduler, an assertion will
-- fail. However, this seems to happen sometimes, somehow. If we get
-- assertion failures from the scheduler code, this is almost
-- certainly why.
lidarlite_TIMEOUT :: ITime
lidarlite_TIMEOUT = toITime (Milliseconds 1000)

lidarliteSensorManager ::
     BackpressureTransmit ('Struct "i2c_transaction_request")
                          ('Struct "i2c_transaction_result")
  -> ChanOutput ('Stored ITime)
  -> ChanInput  ('Struct "lidarlite_sample")
  -> LIDARLite
  -> Tower e ()
lidarliteSensorManager
  (BackpressureTransmit req_chan res_chan)
  init_chan
  sensor_chan
  LIDARLite {..} = do
  let named nm = "lidarlite_" ++ nm
  towerModule lidarliteSampleTypesModule
  towerDepends lidarliteSampleTypesModule
  p <- period (Milliseconds 20) -- 50 hz. Can be faster if required.
  monitor (named "sensor_manager") $ do
    s <- state (named "current_sample")
    last_response <- state (named "last_response")
    -- only enable the samples once the I2C ready chan has fired
    driver_state <- stateInit (named "driver_state") (ival llInitializing)
    coroutineHandler init_chan res_chan (named "coroutine") $ do
      req_e <- emitter req_chan 1
      sens_e <- emitter sensor_chan 1
      return $ CoroutineBody $ \yield_raw -> do
        comment "entry to lidarlite coroutine"

        -- two loops so that we can breakOut when one goes wrong,
        -- while still being ready to service more routines
        forever $ do
          forever $ do
            -- if we ever get a message but are not in the right
            -- driver state, something isn't right, like a timeout, so
            -- exit the loop
            let yield' expected_state = do
                  x <- yield_raw
                  store last_response =<< getTime
                  -- check the driver state and break out if it's unexpected
                  ds <- deref driver_state
                  unless (ds ==? expected_state) $ do
                    -- set an error ourselves
                    store (s ~> samplefail) true
                    breakOut
                  -- update the result code and break out if non-zero
                  rc <- deref (x ~> resultcode)
                  store (s ~> samplefail) (rc >? 0)
                  when (rc >? 0) breakOut
                  return x
                -- the first yield must be for an initial request
                yield0 = yield' llInitialReq
                yield  = yield' llActive

            -- Request originates from period below
            _setup_read_result <- yield0

            store driver_state llActive

            -- wait for LIDAR to be ready
            forever $ do
              ready_req <- fmap constRef $ local $ istruct
                [ tx_addr .= ival lidarlite_i2c_addr
                , tx_buf  .= iarray [
                      -- set register pointer to status register
                      ival 0x01
                    ]
                , tx_len  .= ival 1
                , rx_len  .= ival 0
                ]
              emit req_e ready_req

              _ready_ack <- yield

              ready_read <- fmap constRef $ local $ istruct
                [ tx_addr .= ival lidarlite_i2c_addr
                , tx_buf  .= iarray []
                , tx_len  .= ival 0
                , rx_len  .= ival 1
                ]
              emit req_e ready_read

              ready_sr <- yield
              sr <- deref ((ready_sr ~> rx_buf) ! 0)
              when (sr .& 1 ==? 0) breakOut

            -- bail all the way out if there was an error during the
            -- wait loop
            sf <- deref (s ~> samplefail)
            when sf breakOut

            -- Send request to perform read (see LIDAR-Lite datasheet
            -- for explanation of magic numbers)
            read_tx_req <- fmap constRef $ local $ istruct
              [ tx_addr .= ival lidarlite_i2c_addr
              , tx_buf  .= iarray [
                    -- set register pointer to result register
                    ival 0x8F
                  ]
              , tx_len  .= ival 1
              , rx_len  .= ival 0
              ]
            emit req_e read_tx_req

            _ack <- yield

            read_rx_req <- fmap constRef $ local $ istruct
              [ tx_addr .= ival lidarlite_i2c_addr
              , tx_buf  .= iarray []
              , tx_len  .= ival 0
              , rx_len  .= ival 2
              ]
            emit req_e read_rx_req

            res <- yield
            distance_raw <- payloadu16 res 0 1
            let distance_cal = signCast distance_raw + lidarlite_cal_offset_cm
            store (s ~> distance) (safeCast distance_cal / 100)
            store (s ~> time) =<< timeMicrosFromITime <$> getTime
            breakOut

          -- only get here by breaking out of the main coroutine loop,
          -- which sends the current sample and sets state to inactive
          comment "send sample"
          store driver_state llInactive
          emit sens_e (constRef s)

    handler init_chan (named "ready") $
      callback $ const $ store driver_state llInactive

    handler p (named "periodic_read") $ do
      req_e <- emitter req_chan 1
      callback $ const $ do
        ds <- deref driver_state
        when (ds ==? llInactive) $ do
          -- Initiate a read (see LIDAR-Lite datasheet for explanation
          -- of magic numbers)
          setup_read_req <- fmap constRef $ local $ istruct
            [ tx_addr .= ival lidarlite_i2c_addr
            , tx_buf  .= iarray [
                  -- set register pointer
                  ival 0x00
                  -- acquisition and correlation processing with DC correction
                , ival 0x04
                ]
            , tx_len  .= ival 2
            , rx_len  .= ival 0
            ]
          store driver_state llInitialReq
          emit req_e setup_read_req
        when (ds ==? llInitialReq .|| ds ==? llActive) $ do
          last_t <- deref last_response
          t <- getTime
          when (t - last_t >? lidarlite_TIMEOUT) $ do
            -- dangerous! see comment on lidarlite_TIMEOUT
            store driver_state llInactive
  where
  payloadu16 :: Ref s ('Struct "i2c_transaction_result")
             -> Ix 128 -> Ix 128 -> Ivory eff Uint16
  payloadu16 res ixhi ixlo = do
    hi <- deref ((res ~> rx_buf) ! ixhi)
    lo <- deref ((res ~> rx_buf) ! ixlo)
    assign $ (((safeCast hi `iShiftL` 8) .| safeCast lo) :: Uint16)
