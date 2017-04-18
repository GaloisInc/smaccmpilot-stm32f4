module SMACCMPilot.Hardware.Platforms.ParserUtils where

import Data.Word (Word8)

import Ivory.BSP.STM32.Driver.I2C
import Ivory.Tower.Config

import Prelude ()
import Prelude.Compat

i2cAddr :: ConfigParser I2CDeviceAddr
i2cAddr = subsection "i2caddr" $ do
  addr <- fromIntegral <$> integer
  if 0 <= addr && addr <= (maxBound :: Word8)
    then return (I2CDeviceAddr (fromIntegral addr))
    else fail ("invalid i2c address " ++ show addr)
