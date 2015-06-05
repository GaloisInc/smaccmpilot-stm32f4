{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Hardware.L3GD20.SPI where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.BSP.STM32.Driver.SPI

type Reg = Int

readRegAddr :: Reg -> Uint8
readRegAddr reg = 0x80 .| fromIntegral reg

writeRegAddr :: Reg -> Uint8
writeRegAddr reg = fromIntegral reg

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

regContents :: Ref s (Struct "spi_transaction_result") -> Ivory eff Uint8
regContents res = deref ((res ~> rx_buf) ! 1)

-- | We are not using the L3GD20 sensor found on the Pixhawk at this time.
-- However, we need to sends requests to disable the L3GD20's I2C bus mode,
-- which causes it to corrupt the SPI bus it shares with other devices.
l3gd20Disabler :: BackpressureTransmit (Struct "spi_transaction_request")
                                       (Struct "spi_transaction_result")
               -> ChanOutput (Stored ITime) -- Begin disabling
               -> ChanInput  (Stored ITime) -- Finished successfully
               -> ChanInput  (Stored IBool) -- Unsuccessful: panic
               -> SPIDeviceHandle
               -> Tower e ()
l3gd20Disabler (BackpressureTransmit req_chan res_chan) init_chan init_done panic_chan dev = do
  monitor "l3gd20Ctl" $ do
    retries <- state "l3gd20_retries"
    panicked <- stateInit "l3gd20_panic" (ival false)
    coroutineHandler init_chan res_chan "l3gd20" $ do
      req_e   <- emitter req_chan 1
      done_e  <- emitter init_done 1
      panic_e <- emitter panic_chan 1
      return $ CoroutineBody $ \ yield -> do
        let rpc req = req >>= emit req_e >> yield
        store retries (0 :: Uint8)
        forever $ do
          -- The PX4 project implemented this "disable_i2c" functionality
          -- (commits cb76f07d and 86ec1c37 by Andrew Tridgell) to ensure the
          -- L3GD20 acts as a SPI device, not an I2C device, when connected to
          -- the SPI bus. When it acts like an I2C device, it pulls up the SPI
          -- bus signals and corrupts the data on the bus.  The following
          -- operations are, as far as I know, undocumented in the ST datasheet
          -- and official literature- Tridge's commits say "based on method from
          -- ST engineering support. Shame on you, ST.
          contents_0x05 <- noBreak $ rpc (readRegReq dev 0x05)
          val <- regContents contents_0x05
          _write_0x05 <- noBreak $ rpc (writeRegReq dev 0x05 (val .| 0x20))
          readback_0x05 <- noBreak $ rpc (readRegReq dev 0x05)
          readback_val <- regContents readback_0x05
          when (readback_val ==? (val .| 0x20)) $ do
            breakOut
          rs <- deref retries
          store retries (rs + 1)
          when (rs >? 10) $ do
            store panicked true
            emitV panic_e true
            retVoid

        _ <- rpc $ writeRegReq dev ctl_1 ctl1_val
        _ <- rpc $ writeRegReq dev ctl_2 0
        _ <- rpc $ writeRegReq dev ctl_3 ctl3_drdy_enable
        _ <- rpc $ writeRegReq dev ctl_4 ctl4_bdu
        _ <- rpc $ writeRegReq dev ctl_5 0
        _ <- rpc $ writeRegReq dev ctl_5 ctl5_fifo_enable
        t <- getTime
        emitV done_e t
        retVoid

        where
        ctl_1 = 0x20
        ctl_2 = 0x21
        ctl_3 = 0x22
        ctl_4 = 0x23
        ctl_5 = 0x24

        ctl1_power_normal = 0x08
        ctl1_x_enable     = 0x04
        ctl1_y_enable     = 0x02
        ctl1_z_enable     = 0x01
        ctl1_val = ctl1_power_normal .| ctl1_z_enable .| ctl1_y_enable .| ctl1_x_enable

        ctl3_drdy_enable = 0x08

        ctl4_bdu = 0x80

        ctl5_fifo_enable = 0x40

