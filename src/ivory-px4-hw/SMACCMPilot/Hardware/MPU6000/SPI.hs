{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Hardware.MPU6000.SPI where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.StateMachine
import Ivory.BSP.STM32F4.SPI.Tower.Types
import Ivory.BSP.STM32F4.SPI.Tower.Types.SPIDeviceHandle

import SMACCMPilot.Hardware.MPU6000.Regs

readRegAddr :: Reg -> Uint8
readRegAddr reg = 0x80 .| (fromIntegral (regAddr reg))

writeRegAddr :: Reg -> Uint8
writeRegAddr reg = fromIntegral (regAddr reg)

readRegReq :: (GetAlloc eff ~ Scope s)
           => SPIDeviceHandle
           -> Reg
           -> Ivory eff (ConstRef (Stack s) (Struct "spi_transaction_request"))
readRegReq dev reg = fmap constRef $ local $ istruct
  [ tx_device .= ival dev
  , tx_buf    .= iarray [ ival (readRegAddr reg), ival 0 ]
  , tx_len    .= ival 2
  ]

writeRegReq :: (GetAlloc eff ~ Scope s)
            => SPIDeviceHandle
            -> Reg
            -> Uint8
            -> Ivory eff (ConstRef (Stack s) (Struct "spi_transaction_request"))
writeRegReq dev reg v = fmap constRef $ local $ istruct
  [ tx_device .= ival dev
  , tx_buf    .= iarray [ ival (writeRegAddr reg), ival v ]
  , tx_len    .= ival 2
  ]

getSensorsReq :: (GetAlloc eff ~ Scope s)
              => SPIDeviceHandle
              -> Ivory eff (ConstRef (Stack s) (Struct "spi_transaction_request"))
getSensorsReq dev = fmap constRef $ local $ istruct
  [ tx_device .= ival dev
  , tx_buf    .= iarray [ ival (readRegAddr AccelXoutH) ]
  , tx_len    .= ival 15 -- addr, 6 accel, 2 temp, 6 gyro
  ]


initializerMachine :: SPIDeviceHandle
                   -> ChannelEmitter (Struct "spi_transaction_request")
                   -> Event          (Struct "spi_transaction_result")
                   -> Task p (Runnable, Ref Global (Stored IBool))
initializerMachine dev req_emitter result_evt = do
  failed <- taskLocal "mpu6000InitFailed"

  m <- stateMachine "mpu6000InitailizerMachine" $ mdo
    b <- stateNamed "begin" $ entry $ do
      liftIvory_ $ store failed false
      goto disablei2c

    -- Disable the I2C slave device sharing pins with the SPI interface
    disablei2c <- rpc "disablei2c" (writeRegReq dev UserControl 0x10)
                                   (const (return ()))
                                   whoami

    -- Poll the WhoAmI register. Check for expected result of XXX
    whoami <- rpc "whoami" (readRegReq dev WhoAmI)
                           (\res -> return ()) -- XXX CHECK
                           wake

    -- Wake the sensor device, use internal oscillator
    wake   <- rpc "wake" (writeRegReq dev PowerManagment1 0x00)
                         (const (return ()))
                         setscale

    -- Set gyro scale to +/- 2000 dps
    setscale <- rpc "setscale" (writeRegReq dev GyroConfig 0x18)
                               (const (return ()))
                               done

    done <- stateNamed "done" $ entry $ halt

    return b

  return (m, failed)

  where
  rpc :: String
      -> (forall s1 . Ivory (AllocEffects s1)
                       (ConstRef (Stack s1) (Struct "spi_transaction_request")))
      -> (forall s2 s3 . (ConstRef s2 (Struct "spi_transaction_result"))
            -> Ivory (AllocEffects s3) ())
      -> StateLabel
      -> MachineM StateLabel
  rpc name request resultk statek = mdo
    getter <- stateNamed ("get" ++ name) $ entry $ do
      liftIvory_ $ do
        r <- request
        emit_ req_emitter r
      goto gotResult
    gotResult <- stateNamed ("got" ++ name) $ on result_evt $ \res -> do
      liftIvory_ $ resultk res
      goto statek
    return getter



