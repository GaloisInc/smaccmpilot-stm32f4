{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module SMACCMPilot.Flight.Sensors.PX4Flow where

import Prelude ()
import Prelude.Compat

import Ivory.BSP.STM32.Driver.I2C
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.Config
import Ivory.Tower.HAL.Bus.Interface
import SMACCMPilot.Comm.Ivory.Types.Px4flowSample
import SMACCMPilot.Time
import SMACCMPilot.Hardware.Platforms.ParserUtils

data PX4Flow = PX4Flow {
    px4flow_i2c_addr :: I2CDeviceAddr
  , px4flow_cal_sonar_offset_cm :: Sint16
  }

parsePx4flow :: ConfigParser PX4Flow
parsePx4flow = subsection "px4flow" $ do
  px4flow_i2c_addr <- i2cAddr
  px4flow_cal_sonar_offset_cm
    <- fromIntegral <$> subsection "sonar_offset_cm" integer
  return $ PX4Flow {..}

newtype PX4FlowDriverState = PX4FlowDriverState Uint8
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit, IvoryZeroVal)

px4flowInitializing, px4flowInactive, px4flowInitialReq, px4flowActive :: PX4FlowDriverState
[px4flowInitializing, px4flowInactive, px4flowInitialReq, px4flowActive]
  = map (PX4FlowDriverState . fromInteger) [0..3]

-- | This is dangerous! If the driver tries to send another request
-- while one is already pending from the scheduler, an assertion will
-- fail. However, this seems to happen sometimes, somehow. If we get
-- assertion failures from the scheduler code, this is almost
-- certainly why.
px4flow_TIMEOUT :: ITime
px4flow_TIMEOUT = toITime (Milliseconds 1000)

px4flowSensorManager ::
     BackpressureTransmit ('Struct "i2c_transaction_request")
                          ('Struct "i2c_transaction_result")
  -> ChanOutput ('Stored ITime)
  -> ChanInput  ('Struct "px4flow_sample")
  -> PX4Flow
  -> Tower e ()
px4flowSensorManager
  (BackpressureTransmit req_chan res_chan)
  init_chan
  sensor_chan
  PX4Flow {..} = do
  let named nm = "px4flow_" ++ nm
  towerModule px4flowSampleTypesModule
  towerDepends px4flowSampleTypesModule
  p <- period (Milliseconds 20) -- 50 hz. Can be faster if required.
  monitor (named "sensor_manager") $ do
    s <- state (named "current_sample")
    last_response <- state (named "last_response")
    -- only enable the samples once the I2C ready chan has fired
    driver_state <- stateInit (named "driver_state") (ival px4flowInitializing)
    coroutineHandler init_chan res_chan (named "coroutine") $ do
      req_e <- emitter req_chan 1
      sens_e <- emitter sensor_chan 1
      return $ CoroutineBody $ \yield -> do
        comment "entry to px4flow coroutine"

        -- two loops so that we can breakOut when one goes wrong,
        -- while still being ready to service more routines
        forever $ do
          forever $ do
            -- Request originates from period below
            setup_read_result <- yield
            store last_response =<< getTime
            ds <- deref driver_state

            -- Make sure we're in the right state
            unless (ds ==? px4flowInitialReq) $ do
              store (s ~> samplefail) true
              breakOut
            store driver_state px4flowActive

            -- Fail for I2C error
            rc <- deref (setup_read_result ~> resultcode)
            store (s ~> samplefail) (rc >? 0)
            when (rc >? 0) breakOut

            read_rx_req <- fmap constRef $ local $ istruct
              [ tx_addr .= ival px4flow_i2c_addr
              , tx_buf  .= iarray []
              , tx_len  .= ival 0
              , rx_len  .= ival 22--25 was for integral frame
              ]
            emit req_e read_rx_req

            res <- yield
            store last_response =<< getTime
            ds2 <- deref driver_state

            -- Make sure we're in the right state
            unless (ds2 ==? px4flowActive) $ do
              store (s ~> samplefail) true
              breakOut
            store driver_state px4flowInactive

            -- Unpack read, updating samplefail for I2C error
            rc2 <- deref (res ~> resultcode)
            store (s ~> samplefail) (rc2 >? 0)
            when (rc2 >? 0) breakOut

            store (s ~> frame_count)
              =<< (payloadu16 res 1 0)
            store (s ~> pixel_flow_x_sum)
              =<< (payloads16 res 3 2)
            store (s ~> pixel_flow_y_sum)
              =<< (payloads16 res 5 4)
            store (s ~> flow_comp_m_x)
              =<< (payloads16 res 7 6)
            store (s ~> flow_comp_m_x)
              =<< (payloads16 res 9 8)
            store (s ~> quality)
              =<< (payloads16 res 11 10)
            store (s ~> gyro_x_rate)
              =<< (payloads16 res 13 12)
            store (s ~> gyro_y_rate)
              =<< (payloads16 res 15 14)
            store (s ~> gyro_z_rate)
              =<< (payloads16 res 17 16)
            store (s ~> gyro_range)
              =<< (deref ((res ~> rx_buf) ! 18))
            store (s ~> sonar_timestamp)
              =<< (deref ((res ~> rx_buf) ! 19))
            raw_ground_distance <- payloads16 res 21 20
            store (s ~> ground_distance)
              -- convert offset to meters * 1000 and add it into the raw value
              (raw_ground_distance + (px4flow_cal_sonar_offset_cm * 10))
            store (s ~> time)
              =<< (fmap timeMicrosFromITime getTime)

            --store (s ~> frame_count_since_last_readout)
            --  =<< (payloadu16 res 1 0)
            --store (s ~> pixel_flow_x_integral)
            --  =<< (payloads16 res 3 2)
            --store (s ~> pixel_flow_y_integral)
            --  =<< (payloads16 res 5 4)
            --store (s ~> gyro_x_rate_integral)
            --  =<< (payloads16 res 7 6)
            --store (s ~> gyro_y_rate_integral)
            --  =<< (payloads16 res 9 8)
            --store (s ~> gyro_z_rate_integral)
            --  =<< (payloads16 res 11 10)
            --store (s ~> integration_timespan)
            --  =<< (payloadu32 res 12 13 14 15)
            --store (s ~> sonar_timestamp)
            --  =<< (payloadu32 res 16 17 18 19)
            --store (s ~> ground_distance)
            --  =<< (payloads16 res 21 20)
            --store (s ~> gyro_temperature)
            --  =<< (payloads16 res 23 22)
            --store (s ~> quality)
            --  =<< (deref ((res ~> rx_buf) ! 24))
            --store (s ~> time)
            --  =<< (fmap timeMicrosFromITime getTime)

            -- Send the sample upstream.
            breakOut

          -- only get here by breaking out of the main coroutine loop
          -- which sends the sample and sets the state to inactive
          comment "send sample"
          store driver_state px4flowInactive
          emit sens_e (constRef s)

    handler init_chan (named "ready") $
      callback $ const $ store driver_state px4flowInactive

    handler p (named "periodic_read") $ do
      req_e <- emitter req_chan 1
      callback $ const $ do
        ds <- deref driver_state
        when (ds ==? px4flowInactive) $ do
          store (s ~> samplefail) false
          -- Initiate a read of the integral data frame
          setup_read_req <- fmap constRef $ local $ istruct
            [ tx_addr .= ival px4flow_i2c_addr
            , tx_buf  .= iarray [
                  -- set register pointer for integral frame
                  ival 0x00 -- 0x16 was for integral frame
                ]
            , tx_len  .= ival 1
            , rx_len  .= ival 0
            ]
          store driver_state px4flowInitialReq
          emit req_e setup_read_req
        when (ds ==? px4flowInitialReq .|| ds ==? px4flowActive) $ do
          last_t <- deref last_response
          t <- getTime
          when (t - last_t >? px4flow_TIMEOUT) $ do
            -- dangerous! see comment on px4flow_TIMEOUT
            store driver_state px4flowInactive
  where
  payloadu16 :: Ref s ('Struct "i2c_transaction_result")
             -> Ix 128 -> Ix 128 -> Ivory eff Uint16
  payloadu16 res ixhi ixlo = do
    hi <- deref ((res ~> rx_buf) ! ixhi)
    lo <- deref ((res ~> rx_buf) ! ixlo)
    assign (((safeCast hi `iShiftL` 8) .| safeCast lo) :: Uint16)

  payloads16 :: Ref s ('Struct "i2c_transaction_result")
             -> Ix 128 -> Ix 128 -> Ivory eff Sint16
  payloads16 res ixhi ixlo = do
    hi <- deref ((res ~> rx_buf) ! ixhi)
    lo <- deref ((res ~> rx_buf) ! ixlo)
    assign (twosComplementCast ((safeCast hi `iShiftL` 8) .| safeCast lo) :: Sint16)

  _payloadu32 :: Ref s ('Struct "i2c_transaction_result")
             -> Ix 128 -> Ix 128 -> Ix 128 -> Ix 128 -> Ivory eff Uint32
  _payloadu32 res ixb0 ixb1 ixb2 ixb3 = do
    b0 <- deref ((res ~> rx_buf) ! ixb0)
    b1 <- deref ((res ~> rx_buf) ! ixb1)
    b2 <- deref ((res ~> rx_buf) ! ixb2)
    b3 <- deref ((res ~> rx_buf) ! ixb3)
    assign (((safeCast b3 `iShiftL` 24) .|
             (safeCast b2 `iShiftL` 16) .|
             (safeCast b1 `iShiftL` 8)  .|
              safeCast b0) :: Uint32)
