{-# LANGUAGE DataKinds #-}
module SMACCMPilot.Flight.Sensors.PX4Flow where

import Prelude ()
import Prelude.Compat

import Ivory.BSP.STM32.Driver.I2C
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import SMACCMPilot.Comm.Ivory.Types.Px4flowSample
import SMACCMPilot.Time

data PX4Flow = PX4Flow { px4flow_i2c_addr :: I2CDeviceAddr }

px4flowSensorManager ::
     BackpressureTransmit ('Struct "i2c_transaction_request")
                          ('Struct "i2c_transaction_result")
  -> ChanOutput ('Stored ITime)
  -> ChanInput  ('Struct "px4flow_sample")
  -> I2CDeviceAddr
  -> Tower e ()
px4flowSensorManager
  (BackpressureTransmit req_chan res_chan)
  init_chan
  sensor_chan
  addr = do
  towerModule px4flowSampleTypesModule
  towerDepends px4flowSampleTypesModule
  p <- period (Milliseconds 20) -- 50 hz. Can be faster if required.
  monitor "px4flowSensorManager" $ do
    s <- state "sample"
    -- only enable the samples once the I2C ready chan has fired
    pending <- stateInit "pending" (ival true)
    coroutineHandler init_chan res_chan "px4flow" $ do
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
            is_pending <- deref pending
            assert is_pending
            rc <- deref (setup_read_result ~> resultcode)
            -- Reset the samplefail field
            store (s ~> samplefail) (rc >? 0)
            when (rc >? 0) breakOut

            read_rx_req <- fmap constRef $ local $ istruct
              [ tx_addr .= ival addr
              , tx_buf  .= iarray []
              , tx_len  .= ival 0
              , rx_len  .= ival 22--25 was for integral frame
              ]
            emit req_e read_rx_req
            res <- yield
            store pending false
            -- Unpack read, updating samplefail if failed.
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
            store (s ~> ground_distance)
              =<< (payloads16 res 21 20)
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
          -- Initiate a read of the integral data frame
          setup_read_req <- fmap constRef $ local $ istruct
            [ tx_addr .= ival addr
            , tx_buf  .= iarray [
                  -- set register pointer for integral frame
                  ival 0x00 -- 0x16 was for integral frame
                ]
            , tx_len  .= ival 1
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
    assign (((safeCast hi `iShiftL` 8) .| safeCast lo) :: Uint16)

  payloads16 :: Ref s ('Struct "i2c_transaction_result")
             -> Ix 128 -> Ix 128 -> Ivory eff Sint16
  payloads16 res ixhi ixlo = do
    hi <- deref ((res ~> rx_buf) ! ixhi)
    lo <- deref ((res ~> rx_buf) ! ixlo)
    assign (twosComplementCast ((safeCast hi `iShiftL` 8) .| safeCast lo) :: Sint16)

  payloadu32 :: Ref s ('Struct "i2c_transaction_result")
             -> Ix 128 -> Ix 128 -> Ix 128 -> Ix 128 -> Ivory eff Uint32
  payloadu32 res ixb0 ixb1 ixb2 ixb3 = do
    b0 <- deref ((res ~> rx_buf) ! ixb0)
    b1 <- deref ((res ~> rx_buf) ! ixb1)
    b2 <- deref ((res ~> rx_buf) ! ixb2)
    b3 <- deref ((res ~> rx_buf) ! ixb3)
    assign (((safeCast b3 `iShiftL` 24) .|
             (safeCast b2 `iShiftL` 16) .|
             (safeCast b1 `iShiftL` 8)  .|
              safeCast b0) :: Uint32)
