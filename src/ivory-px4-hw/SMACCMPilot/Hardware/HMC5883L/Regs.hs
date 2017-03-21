
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

data SampleAveraging
  = NoAveraging
  | Average2
  | Average4
  | Average8
  deriving (Eq, Show)

data OutputRate
  = Rate0_75hz
  | Rate1_5hz
  | Rate3hz
  | Rate7_5hz
  | Rate15hz
  | Rate30hz
  | Rate75hz
  deriving (Eq, Show)

data BiasMode
  = NoBias
  | PositiveBias
  | NegativeBias

confAVal :: SampleAveraging -> OutputRate -> BiasMode -> Word8
confAVal a o b =  (savg  a `shiftL` 5)
              .|. (orate o `shiftL` 2)
              .|. (bmode b `shiftL` 0)
  where
  savg NoAveraging = 0
  savg Average2    = 1
  savg Average4    = 2
  savg Average8    = 3

  orate Rate0_75hz = 0
  orate Rate1_5hz  = 1
  orate Rate3hz    = 2
  orate Rate7_5hz  = 3
  orate Rate15hz   = 4
  orate Rate30hz   = 5
  orate Rate75hz   = 6

  bmode NoBias       = 0
  bmode PositiveBias = 1
  bmode NegativeBias = 2

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

confBVal :: Gain -> Word8
confBVal g = bs g `shiftL` 4
  where
  bs LSBGauss1370 = 0
  bs LSBGauss1090 = 1
  bs LSBGauss820  = 2
  bs LSBGauss660  = 3
  bs LSBGauss440  = 4
  bs LSBGauss390  = 5
  bs LSBGauss330  = 6
  bs LSBGauss230  = 7

data MeasMode
  = Continuous
  | Single
  | Idle
  deriving (Eq, Show)

modeVal :: MeasMode -> Word8
modeVal Continuous = 0
modeVal Single     = 1
modeVal Idle       = 2

