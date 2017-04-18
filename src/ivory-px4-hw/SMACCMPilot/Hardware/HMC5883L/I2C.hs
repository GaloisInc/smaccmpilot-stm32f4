{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

module SMACCMPilot.Hardware.HMC5883L.I2C where

import Prelude ()
import Prelude.Compat

import Data.Word
import Ivory.BSP.STM32.Driver.I2C
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.Config
import Ivory.Tower.HAL.Bus.Interface
import SMACCMPilot.Comm.Ivory.Types.MagnetometerSample
import SMACCMPilot.Comm.Ivory.Types.Xyz
import SMACCMPilot.Hardware.HMC5883L.Regs
import SMACCMPilot.Hardware.Platforms.ParserUtils
import SMACCMPilot.Hardware.Sensors
import SMACCMPilot.Time

data HMC5883L = HMC5883L {
    hmc5883l_i2c_addr :: I2CDeviceAddr
  , hmc5883l_mag_cal :: MagCal
  }

parseHMC5883L :: ConfigParser HMC5883L
parseHMC5883L = do
  hmc5883l_i2c_addr <- subsection "hmc5883l" i2cAddr
  hmc5883l_mag_cal <- parseMagCal "hmc5883l"
  return HMC5883L {..}

named :: String -> String
named nm = "hmc5883l_" ++ nm

hmc5883lSensorManager :: MagCal
                      -> BackpressureTransmit ('Struct "i2c_transaction_request") ('Struct "i2c_transaction_result")
                      -> ChanOutput ('Stored ITime)
                      -> ChanInput  ('Struct "magnetometer_sample")
                      -> I2CDeviceAddr
                      -> Tower e ()
hmc5883lSensorManager (MagCal mag_cal) (BackpressureTransmit req_chan res_chan) init_chan sensor_chan addr = do
  towerModule magnetometerSampleTypesModule
  towerDepends magnetometerSampleTypesModule
  towerModule xyzTypesModule
  towerDepends xyzTypesModule
  p <- period (Milliseconds 20) -- 50 hz. Can be faster if required.
  monitor (named "sensor_manager") $ do
    init_requests_area <- do
      let reqs = iarray
            [ regWriteInit addr ConfA $ confAVal Average8 Rate75hz NoBias
            , regWriteInit addr ConfB $ confBVal LSBGauss1090
            , regWriteInit addr Mode  $ modeVal  Continuous
            ] :: Init ('Array 3 ('Struct "i2c_transaction_request"))
      constArea <$> fmap showUnique (freshname "hmc5883l_init_requests") <*> pure reqs
    monitorModuleDef $ defConstMemArea init_requests_area
    let init_requests = addrOf init_requests_area

    initialized <- stateInit (named "initialized") (ival false)
    active      <- stateInit (named "active")      (ival false)
    s           <- state (named "sample")
    coroutineHandler init_chan res_chan (named "coroutine") $ do
      req_e <- emitter req_chan 1
      sens_e <- emitter sensor_chan 1
      return $ CoroutineBody $ \yield -> do
        comment "entry to hmc5883l coroutine"
        forever $ do
          store (s ~> initfail) false
          arrayMap $ \ i -> do
            emit req_e $ init_requests ! i
            res <- yield
            code <- deref (res ~> resultcode)
            -- Set the initfail field if i2c failed
            when (code >? 0) (store (s ~> initfail) true)
          failed <- deref (s ~> initfail)
          unless failed breakOut
        store initialized true
        comment "finished initializing in hmc5883l coroutine"

        forever $ do
          -- Request originates from period below
          setup_read_result <- yield
          comment "response received from periodic read"
          rc <- deref (setup_read_result ~> resultcode)
          -- Reset the samplefail field
          store (s ~> samplefail) (rc >? 0)
          -- Send request to perform read
          do_read_req <- fmap constRef $ local $ istruct
            [ tx_addr .= ival addr
            , tx_buf  .= iarray []
            , tx_len  .= ival 0
            , rx_len  .= ival 6
            ]
          emit req_e do_read_req
          res <- yield
          comment "response received from perform read request"
          store active false
          -- Unpack read, updating samplefail if failed.
          rc2 <- deref (res ~> resultcode)
          when (rc2 >? 0) (store (s ~> samplefail) true)
          let (cal_x, cal_y, cal_z) = applyXyzCal mag_cal
          payloads16 res 0 1 >>= store ((s ~> sample) ~> y) . cal_y -- xh, xl
          payloads16 res 2 3 >>= store ((s ~> sample) ~> z) . cal_z -- zh, zl
          payloads16 res 4 5 >>= store ((s ~> sample) ~> x) . cal_x -- yh, yl
          fmap timeMicrosFromITime getTime >>= store (s ~> time)
          -- Send the sample upstream.
          emit sens_e (constRef s)


    handler p (named "periodic_read") $ do
      req_e <- emitter req_chan 1
      callback $ const $ do
        i <- deref initialized
        a <- deref active
        when (i .&& iNot a) $ do
          -- Initiate a read
          setup_read_req <- fmap constRef $ local $ istruct
            [ tx_addr .= ival addr
            , tx_buf  .= iarray [ ival (fromIntegral (regAddr OutXH)) ]
            , tx_len  .= ival 1
            , rx_len  .= ival 0
            ]
          store active true
          emit req_e setup_read_req
  where
  payloads16 :: Ref s ('Struct "i2c_transaction_result")
             -> Ix 128 -> Ix 128 -> Ivory eff Sint16
  payloads16 res ixhi ixlo = do
    hi <- deref ((res ~> rx_buf) ! ixhi)
    lo <- deref ((res ~> rx_buf) ! ixlo)
    assign $ twosComplementCast ((safeCast hi `iShiftL` 8) .| safeCast lo)

regWriteInit :: I2CDeviceAddr
             -> Reg
             -> Word8
             -> Init ('Struct "i2c_transaction_request")
regWriteInit addr r v = istruct
  [ tx_addr .= ival addr
  , tx_buf  .= iarray [ ival (fromIntegral (regAddr r)), ival (fromIntegral v) ]
  , tx_len  .= ival 2
  , rx_len  .= ival 0
  ]

