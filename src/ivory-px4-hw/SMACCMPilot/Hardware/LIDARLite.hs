{-# LANGUAGE DataKinds #-}
module SMACCMPilot.Hardware.LIDARLite where


import Prelude ()
import Prelude.Compat

import Ivory.BSP.STM32.Driver.I2C
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import SMACCMPilot.Comm.Ivory.Types.LidarliteSample
import SMACCMPilot.Time

data LIDARLite = LIDARLite { lidarlite_i2c_addr :: I2CDeviceAddr }

lidarliteSensorManager ::
     BackpressureTransmit ('Struct "i2c_transaction_request")
                          ('Struct "i2c_transaction_result")
  -> ChanOutput ('Stored ITime)
  -> ChanInput  ('Struct "lidarlite_sample")
  -> I2CDeviceAddr
  -> Tower e ()
lidarliteSensorManager
  (BackpressureTransmit req_chan res_chan)
  init_chan
  sensor_chan
  addr = do
  towerModule lidarliteSampleTypesModule
  towerDepends lidarliteSampleTypesModule
  p <- period (Milliseconds 20) -- 50 hz. Can be faster if required.
  monitor "lidarliteSensorManager" $ do
    s <- state "sample"
    -- only enable the samples once the I2C ready chan has fired
    pending <- stateInit "pending" (ival true)
    coroutineHandler init_chan res_chan "lidarlite" $ do
      req_e <- emitter req_chan 1
      sens_e <- emitter sensor_chan 1
      return $ CoroutineBody $ \yield -> do
        comment "entry to lidarlite coroutine"

        -- two loops so that we can breakOut when one goes wrong,
        -- while still being ready to service more routines
        forever $ do
          forever $ do
            -- Request originates from period below
            setup_read_result <- yield
            is_pending <- deref pending
            assert is_pending
            rc <- deref (setup_read_result ~> resultcode)
            -- Reset the samplefail field
            store (s ~> samplefail) (rc >? 0)
            when (rc >? 0) breakOut

            -- wait for LIDAR to be ready
            forever $ do
              ready_req <- fmap constRef $ local $ istruct
                [ tx_addr .= ival addr
                , tx_buf  .= iarray [
                      -- set register pointer to status register
                      ival 0x01
                    ]
                , tx_len  .= ival 1
                , rx_len  .= ival 0
                ]
              emit req_e ready_req

              ready_ack <- yield
              rc_readyack <- deref (ready_ack ~> resultcode)
              store (s ~> samplefail) (rc_readyack >? 0)
              when (rc_readyack >? 0) breakOut

              ready_read <- fmap constRef $ local $ istruct
                [ tx_addr .= ival addr
                , tx_buf  .= iarray []
                , tx_len  .= ival 0
                , rx_len  .= ival 1
                ]
              emit req_e ready_read

              ready_sr <- yield
              rc_readysr <- deref (ready_sr ~> resultcode)
              store (s ~> samplefail) (rc_readysr >? 0)
              when (rc_readysr >? 0) breakOut

              sr <- deref ((ready_sr ~> rx_buf) ! 0)
              when (sr .& 1 ==? 0) breakOut

            -- bail all the way out if there's an error
            sf <- deref (s ~> samplefail)
            when sf breakOut

            -- Send request to perform read (see LIDAR-Lite datasheet
            -- for explanation of magic numbers)
            read_tx_req <- fmap constRef $ local $ istruct
              [ tx_addr .= ival addr
              , tx_buf  .= iarray [
                    -- set register pointer to result register
                    ival 0x8F
                  ]
              , tx_len  .= ival 1
              , rx_len  .= ival 0
              ]
            emit req_e read_tx_req
            ack <- yield
            rc2 <- deref (ack ~> resultcode)
            store (s ~> samplefail) (rc2 >? 0)
            when (rc2 >? 0) breakOut

            read_rx_req <- fmap constRef $ local $ istruct
              [ tx_addr .= ival addr
              , tx_buf  .= iarray []
              , tx_len  .= ival 0
              , rx_len  .= ival 2
              ]
            emit req_e read_rx_req
            res <- yield
            store pending false
            -- Unpack read, updating samplefail if failed.
            rc3 <- deref (res ~> resultcode)
            store (s ~> samplefail) (rc3 >? 0)
            when (rc3 >? 0) breakOut

            distance_raw <- payloadu16 res 0 1
            store (s ~> distance) (safeCast distance_raw / 100)
            fmap timeMicrosFromITime getTime >>= store (s ~> time)
            -- Send the sample upstream.
            emit sens_e (constRef s)

          -- only get here by breaking out of the main coroutine
          -- loop. reset the state and get ready for another sample
          comment "error handling"
          store pending false
          -- emit result with error code
          emit sens_e (constRef s)

    handler init_chan "lidar_ready" $
      callback $ const $ store pending false

    handler p "periodic_read" $ do
      req_e <- emitter req_chan 1
      callback $ const $ do
        is_pending <- deref pending
        unless is_pending $ do
          -- Initiate a read (see LIDAR-Lite datasheet for explanation
          -- of magic numbers)
          setup_read_req <- fmap constRef $ local $ istruct
            [ tx_addr .= ival addr
            , tx_buf  .= iarray [
                  -- set register pointer
                  ival 0x00
                  -- acquisition and correlation processing with DC correction
                , ival 0x04
                ]
            , tx_len  .= ival 2
            , rx_len  .= ival 0
            ]
          store pending true
          emit req_e setup_read_req
  where
  payloadu16 :: Ref s ('Struct "i2c_transaction_result")
             -> Ix 128 -> Ix 128 -> Ivory eff Uint16
  payloadu16 res ixhi ixlo = do
    hi <- deref ((res ~> rx_buf) ! ixhi)
    lo <- deref ((res ~> rx_buf) ! ixlo)
    assign $ (((safeCast hi `iShiftL` 8) .| safeCast lo) :: Uint16)
