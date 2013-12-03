{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module MPU6000 where

import Ivory.Language
import Ivory.Tower

import Ivory.BSP.STM32F4.SPI.Tower

getWhoAmI :: (SingI n, GetAlloc eff ~ Scope s)
          => ChannelEmitter n (Struct "spi_transmission")
          -> Ivory eff ()
getWhoAmI ch = do
  tx <- readReg 0x75
  emit_ ch tx

disableI2C :: (SingI n, GetAlloc eff ~ Scope s)
           => ChannelEmitter n (Struct "spi_transmission")
           -> Ivory eff ()
disableI2C ch = do
  tx <- writeReg 0x6A 0x10 -- disable i2c interface (shares pins with spi)
  emit_ ch tx

wake :: (SingI n, GetAlloc eff ~ Scope s)
        => ChannelEmitter n (Struct "spi_transmission")
        -> Ivory eff ()
wake ch = do
  tx <- writeReg 0x6B 0x00 -- wake up, use internal oscillator
  emit_ ch tx

setScale :: (SingI n, GetAlloc eff ~ Scope s)
         => ChannelEmitter n (Struct "spi_transmission")
         -> Ivory eff ()
setScale ch = do
  tx <- writeReg 0x1B 0x18 -- Gyro at +/- 2000dps
  emit_ ch tx


getSensors :: (SingI n, GetAlloc eff ~ Scope s)
           => ChannelEmitter n (Struct "spi_transmission")
           -> Ivory eff ()
getSensors ch = do
  tx <- local s 
  emit_ ch (constRef tx)
  where
  reg = 0x3b .| 0x80
  s = istruct [tx_buf .= iarray [ ival reg ], tx_len .= ival 16 ]

-- Internal Helper Functions ---------------------------------------------------

readReg :: (GetAlloc eff ~ Scope s)
         => Uint8
         -> Ivory eff (ConstRef (Stack s) (Struct "spi_transmission"))
readReg reg = writeReg (0x80 .| reg) 0

writeReg :: (GetAlloc eff ~ Scope s)
         => Uint8
         -> Uint8
         -> Ivory eff (ConstRef (Stack s) (Struct "spi_transmission"))
writeReg reg value = local s >>= (return . constRef)
  where
  s = istruct [tx_buf .= iarray [ ival reg, ival value], tx_len .= ival 2]

