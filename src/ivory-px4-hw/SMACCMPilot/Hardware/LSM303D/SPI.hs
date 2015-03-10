{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.LSM303D.SPI
  ( lsm303dSPISensorManager
  , lsm303dDefaultConf
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import Data.Word
import Numeric (showHex)

import Ivory.BSP.STM32.Driver.SPI

import SMACCMPilot.Hardware.LSM303D.Regs
import SMACCMPilot.Hardware.LSM303D.Types

lsm303dSPISensorManager :: Config
                        -> ChanInput  (Struct "spi_transaction_request")
                        -> ChanOutput (Struct "spi_transaction_result")
                        -> ChanOutput (Stored ITime)
                        -> ChanInput  (Struct "lsm303d_sample")
                        -> SPIDeviceHandle
                        -> Tower e ()
lsm303dSPISensorManager conf req_chan res_chan init_chan sensor_chan h = do
  towerModule  lsm303dTypesModule
  towerDepends lsm303dTypesModule
  p <- period (Milliseconds 20) -- 50hz
  monitor "lsm303dSensorManager" $ do
    initialized <- stateInit "initialized" (ival false)
    s           <- state "sample"
    coroutineHandler init_chan res_chan "lsm303d_coroutine" $ do
      req_e <- emitter req_chan 1
      sens_e <- emitter sensor_chan 1
      return $ CoroutineBody $ \yield -> do

        let runA = runAction h req_e yield (s ~> initfail)
            initActions :: Config -> [RegAction]
            initActions c =
              [ RegRead    R_WhoAmI $ \r -> do
                  whoami <- deref ((r ~> rx_buf) ! 1)
                  return (whoami ==? 0x49)
              , RegModify R_Magic1 (\r -> r .| 0x10)
              , RegModify R_Magic1 (\r -> r .& 0xf7)
              , RegModify R_Magic2 (\r -> r .| 0x80)
              , RegModify R_Magic1 (\r -> r .& 0xe7)
              , RegWrite R_Control1 $ control1Val $ conf_ctl1 c
              , RegWrite R_Control2 $ control2Val $ conf_ctl2 c
              , RegWrite R_Control5 $ control5Val $ conf_ctl5 c
              , RegWrite R_Control6 $ control6Val $ conf_ctl6 c
              , RegWrite R_Control7 $ control7Val $ conf_ctl7 c
              ]

        comment "begin initialization in lsm303d coroutine"
        mapM_ runA (initActions conf)

        store initialized true
        comment "finished initialization in lsm303d coroutine"

        forever $ do
          comment "Recieve mag read result."
          comment "Request originates from periodic handler below."
          mag_read_result <- yield

          comment "reset samplefail field"
          mrc <- deref (mag_read_result ~> resultcode)
          store (s ~> samplefail) (mrc >? 0)

          comment "put results in mag_sample field"
          convert_mag_sample conf mag_read_result s

          comment "send accel read request"
          acc_read_req <- fmap constRef $ local $ istruct
            [ tx_device .= ival h
            , tx_buf  .= iarray ((ival (fromIntegral (readSequentialRegs R_OutXLA))) :
                                    repeat (ival 0))
            , tx_len  .= ival 7
            ]
          emit req_e acc_read_req

          comment "receive accel read result"
          acc_read_result <- yield

          comment "update samplefail field"
          arc <- deref (acc_read_result ~> resultcode)
          when (arc >? 0) $ store (s ~> samplefail) true

          comment "put results in acc_sample field"
          convert_acc_sample conf acc_read_result s

          comment "record time and emit sample"
          getTime >>= store (s ~> time)
          emit sens_e (constRef s)


    handler p "periodic_read" $ do
      req_e <- emitter req_chan 1
      callback $ const $ do
        i <- deref initialized
        when i $ do
          do_read_req <- fmap constRef $ local $ istruct
            [ tx_device .= ival h
            , tx_buf  .= iarray ((ival (fromIntegral (readSequentialRegs R_OutXLM))) :
                                    repeat (ival 0))
            , tx_len  .= ival 7
            ]
          emit req_e do_read_req

convert_mag_sample :: Config
                   -> Ref s1 (Struct "spi_transaction_result")
                   -> Ref s2 (Struct "lsm303d_sample")
                   -> Ivory eff ()
convert_mag_sample c res s = convert_sample scale res (s ~> mag_sample)
  where
  scale :: IFloat -> IFloat
  scale v = v * (fromInteger (magPeakToPeakGauss c))
              / (fromInteger (2 ^ (16 :: Int)))

convert_acc_sample :: Config
                   -> Ref s1 (Struct "spi_transaction_result")
                   -> Ref s2 (Struct "lsm303d_sample")
                   -> Ivory eff ()
convert_acc_sample c res s = convert_sample scale res (s ~> acc_sample)
  where
  scale :: IFloat -> IFloat
  scale v = v * (fromRational (accPeakToPeakMSS c))
              / (fromInteger (2 ^ (16 :: Int)))

convert_sample :: (IFloat -> IFloat)
               -> Ref s1 (Struct "spi_transaction_result")
               -> Ref s2 (Array 3 (Stored IFloat))
               -> Ivory eff ()
convert_sample scale res s = do
  f ((res ~> rx_buf) ! 1)
    ((res ~> rx_buf) ! 2)
    (s ! 0)
  f ((res ~> rx_buf) ! 3)
    ((res ~> rx_buf) ! 4)
    (s ! 1)
  f ((res ~> rx_buf) ! 5)
    ((res ~> rx_buf) ! 6)
    (s ! 2)
  where
  f loref hiref resref = do
    lo <- deref loref
    hi <- deref hiref
    (u16 :: Uint16) <- assign ((safeCast lo) + ((safeCast hi) `iShiftL` 8))
    (i16 :: Sint16) <- assign (twosComplementCast u16)
    (r :: IFloat)   <- assign (scale (safeCast i16))
    store resref r


lsm303dDefaultConf :: Config
lsm303dDefaultConf = Config
  { conf_ctl1 = Control1
      { accel_datarate = ADR_800hz
      , block_data_update = True
      , accel_x_enable = True
      , accel_y_enable = True
      , accel_z_enable = True
      }
  , conf_ctl2 = Control2
      { accel_anti_alias_bw = AABW_50hz
      , accel_full_scale = AAFS_8g
      , accel_self_test = False
      , spi_3wire_mode = False
      }
  , conf_ctl5 = Control5
      { temp_enable = False
      , mag_resolution = MR_High
      , mag_datarate = MDR_100hz
      }
  , conf_ctl6 = Control6
      { mag_full_scale = MFS_2gauss
      }
  , conf_ctl7 = Control7
      { mag_power_mode = MPM_Continuous
      }
  }

data RegAction
  = RegWrite Reg Word8
  | RegRead Reg (forall s eff . Ref s (Struct "spi_transaction_result")
                  -> Ivory eff IBool) -- Test successful
  | RegModify Reg (Uint8 -> Uint8)

runAction :: (GetAlloc eff ~ Scope s)
          => SPIDeviceHandle
          -> Emitter (Struct "spi_transaction_request")
          -> (Ivory eff (Ref s1 (Struct "spi_transaction_result")))
          -> Ref s2 (Stored IBool)
          -> RegAction
          -> Ivory eff ()
runAction h e y failed (RegModify r f) = do
  comment ("RegModify " ++ show r)
  read_req <- readTransaction h r
  emit e read_req
  read_res <- yieldCheckResultcode y failed
  read_val <- deref ((read_res ~> rx_buf) ! 1)
  write_req <- writeTransaction h r (f read_val)
  emit e write_req
  _write_res <- yieldCheckResultcode y failed
  return ()

runAction h e y failed (RegWrite  r v) = do
  comment ("RegWrite " ++ show r ++ " 0x" ++ showHex (writeReg r) " 0x" ++ showHex v "")
  write_req <- writeTransaction h r (fromIntegral v)
  emit e write_req
  _write_res <- yieldCheckResultcode y failed
  return ()

runAction h e y failed (RegRead  r f) = do
  comment ("RegRead " ++ show r)
  read_req <- readTransaction h r
  emit e read_req
  read_res <- yieldCheckResultcode y failed
  success <- f read_res
  unless success (store failed true)

yieldCheckResultcode :: Ivory eff (Ref s1 (Struct "spi_transaction_result"))
          -> Ref s2 (Stored IBool)
          -> Ivory eff (Ref s1 (Struct "spi_transaction_result"))
yieldCheckResultcode y f = do
  res <- y
  rc <- deref (res ~> resultcode)
  when (rc >? 0) (store f true)
  return res


readTransaction :: (GetAlloc eff ~ Scope s)
                => SPIDeviceHandle -> Reg
                -> Ivory eff (ConstRef (Stack s) (Struct "spi_transaction_request"))
readTransaction h r = fmap constRef $ local $ istruct
  [ tx_device .= ival h
  , tx_buf .= iarray [ ival (fromIntegral (readReg r)) , ival 0 ]
  , tx_len .= ival 2
  ]

writeTransaction :: (GetAlloc eff ~ Scope s)
                 => SPIDeviceHandle -> Reg -> Uint8
                 -> Ivory eff (ConstRef (Stack s) (Struct "spi_transaction_request"))
writeTransaction h r v = fmap constRef $ local $ istruct
  [ tx_device .= ival h
  , tx_buf .= iarray [ ival (fromIntegral (writeReg r)) , ival v ]
  , tx_len .= ival 2
  ]
