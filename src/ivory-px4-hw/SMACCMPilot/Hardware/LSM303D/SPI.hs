{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.LSM303D.SPI
  ( lsm303dSPISensorManager
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import Data.Word

import Ivory.BSP.STM32.Driver.SPI

import SMACCMPilot.Hardware.LSM303D.Regs
import SMACCMPilot.Hardware.LSM303D.Types

lsm303dSPISensorManager :: ChanInput  (Struct "spi_transaction_request")
                        -> ChanOutput (Struct "spi_transaction_result")
                        -> ChanOutput (Stored ITime)
                        -> ChanInput  (Struct "lsm303d_sample")
                        -> SPIDeviceHandle
                        -> Tower e ()
lsm303dSPISensorManager req_chan res_chan init_chan sensor_chan h = do
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
        comment "entry to lsm303d coroutine"
        let onresult res = do
              rc <- deref (res ~> resultcode)
              store (s ~> initfail) (rc >? 0)
              return res
        mapM_ (runAction h req_e (yield >>= onresult)) actions

        store initialized true
        comment "finished initializing in lsm303d coroutine"

        forever $ do
          -- Request originates from period below
          mag_read_result <- yield
          rc <- deref (mag_read_result ~> resultcode)
          -- Reset the samplefail field
          store (s ~> samplefail) (rc >? 0)

          let m_samp loref hiref resref = do
                lo <- deref loref
                hi <- deref hiref
                -- XXX endian might be wrong?
                -- XXX units are definitely wrong.
                store resref ((safeCast lo) + ((safeCast hi) * 256))

          m_samp ((mag_read_result ~> rx_buf) ! 1)
                 ((mag_read_result ~> rx_buf) ! 2)
                 ((s ~> mag_sample) ! 0)
          m_samp ((mag_read_result ~> rx_buf) ! 3)
                 ((mag_read_result ~> rx_buf) ! 4)
                 ((s ~> mag_sample) ! 1)
          m_samp ((mag_read_result ~> rx_buf) ! 5)
                 ((mag_read_result ~> rx_buf) ! 6)
                 ((s ~> mag_sample) ! 2)
          -- XXX do another transaction to read the accelerometer registers
          getTime >>= store (s ~> time)
          -- Send the sample upstream.
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


data RegAction
  = RegWrite Reg Word8
  | RegRead Reg (forall s eff . Ref s (Struct "spi_transaction_result") -> Ivory eff IBool)
  | RegModify Reg (Uint8 -> Uint8)

actions :: [RegAction]
actions =
  [ RegRead    R_WhoAmI $ \r -> do
      comment "break here!"
      whoami <- deref ((r ~> rx_buf) ! 1)
      return (whoami ==? 0x49)

  --, RegWrite   R_Control0 $ control0Val $ Control0
  --    { reboot = True
  --    }
  , RegModify R_Magic1 (\r -> r .| 0x10)
  , RegModify R_Magic1 (\r -> r .& 0xf7)
  , RegModify R_Magic2 (\r -> r .| 0x80)
  , RegModify R_Magic1 (\r -> r .& 0xe7)
  , RegWrite   R_Control1 $ control1Val $ Control1
      { accel_datarate = ADR_800hz
      , block_data_update = True
      , accel_x_enable = True
      , accel_y_enable = True
      , accel_z_enable = True
      }
  , RegWrite   R_Control2 $ control2Val $ Control2
      { accel_anti_alias_bw = AABW_50hz
      , accel_full_scale = AAFS_8g
      , accel_self_test = False
      , spi_3wire_mode = False
      }
  , RegWrite   R_Control5 $ control5Val $ Control5
      { temp_enable = False
      , mag_resolution = MR_High
      , mag_datarate = MDR_100hz
      }
  , RegWrite   R_Control6 $ control6Val $ Control6
      { mag_full_scale = MFS_2gauss
      }
  , RegWrite   R_Control7 $ control7Val $ Control7
      { accel_filter_enable = True
      , mag_power_mode = MPM_Continuous
      }
  ]

runAction :: (GetAlloc eff ~ Scope s)
          => SPIDeviceHandle
          -> Emitter (Struct "spi_transaction_request")
          -> (Ivory eff (Ref s2 (Struct "spi_transaction_result")))
          -> RegAction
          -> Ivory eff ()
runAction h e y (RegModify r f) = do
  comment ("RegModify " ++ show r)
  read_req <- readTransaction h r
  emit e read_req
  read_res <- y
  read_val <- deref ((read_res ~> rx_buf) ! 1)
  write_req <- writeTransaction h r (f read_val)
  emit e write_req
  _write_res <- y
  return ()

runAction h e y (RegWrite  r v) = do
  comment ("RegWrite " ++ show r)
  write_req <- writeTransaction h r (fromIntegral v)
  emit e write_req
  _write_res <- y
  return ()

runAction h e y (RegRead  r f) = do
  comment ("RegRead " ++ show r)
  read_req <- readTransaction h r
  emit e read_req
  read_res <- y
  _ <- f read_res
  -- XXX do something with result of f here - set initfail if false!
  return ()


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
