
module SMACCMPilot.Hardware.HMC5883L.Regs where

import Data.Bits
import Data.Word

data Reg
  = ConfA
  | ConfB
  | Mode
  | OutXH
  | OutXL
  | OutZH
  | OutZL
  | OutYH
  | OutYL
  | Status
  | IdentA
  | IdentB
  | IdentC
  deriving (Eq, Show)


regAddr :: Reg -> Word8
regAddr ConfA  = 0
regAddr ConfB  = 1
regAddr Mode   = 2
regAddr OutXH  = 3
regAddr OutXL  = 4
regAddr OutZH  = 5
regAddr OutZL  = 6
regAddr OutYH  = 7
regAddr OutYL  = 8
regAddr Status = 9
regAddr IdentA = 10
regAddr IdentB = 11
regAddr IdentC = 12

data OutputRate
  = Rate0_75hz
  | Rate1_5hz
  | Rate3hz
  | Rate7_5hz
  | Rate15hz
  | Rate30hz
  | Rate75Hz
  deriving (Eq, Show)


-- Ignoring the other settings in confA because I don't need them (leaving zero)
confA :: OutputRate -> Word8
confA o = bs o `shiftL` 2
  where
  bs Rate0_75hz = 0
  bs Rate1_5hz  = 1
  bs Rate3hz    = 2
  bs Rate7_5hz  = 3
  bs Rate15hz   = 4
  bs Rate30hz   = 5
  bs Rate75Hz   = 6

data Gain
  = LSBGauss1370
  | LSBGauss1090
  | LSBGauss820
  | LSBGauss660
  | LSBGauss440
  | LSBGauss390
  | LSBGauss330
  | LSBGauss230
  deriving (Eq, Show)

confB :: Gain -> Word8
confB g = bs g `shiftL` 4
  where
  bs LSBGauss1370 = 0
  bs LSBGauss1090 = 1
  bs LSBGauss820  = 2
  bs LSBGauss660  = 3
  bs LSBGauss440  = 4
  bs LSBGauss390  = 5
  bs LSBGauss330  = 6
  bs LSBGauss230  = 7


