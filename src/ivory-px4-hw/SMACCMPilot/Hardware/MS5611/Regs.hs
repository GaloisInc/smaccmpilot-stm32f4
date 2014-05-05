
module SMACCMPilot.Hardware.MS5611.Regs where

import Data.Bits
import Data.Word

data Command
  = Reset
  | ConvertD1 OSR
  | ConvertD2 OSR
  | ADCRead
  | PromRead PROM
  deriving (Eq, Show)

data OSR
  = OSR256
  | OSR512
  | OSR1024
  | OSR2048
  | OSR4096
  deriving (Eq, Show)

data PROM
  = Coeff1
  | Coeff2
  | Coeff3
  | Coeff4
  | Coeff5
  | Coeff6
  deriving (Eq, Show)

commandVal :: Command -> Word8
commandVal Reset           = 0x1E
commandVal (ConvertD1 osr) = 0x40 .|. osrBits osr
commandVal (ConvertD2 osr) = 0x50 .|. osrBits osr
commandVal ADCRead         = 0x00
commandVal (PromRead prom) = 0xA0 .|. promBits prom

osrBits :: OSR -> Word8
osrBits OSR256  = 0
osrBits OSR512  = 2
osrBits OSR1024 = 4
osrBits OSR2048 = 6
osrBits OSR4096 = 8

promBits :: PROM -> Word8
promBits Coeff1 = 2
promBits Coeff2 = 4
promBits Coeff3 = 6
promBits Coeff4 = 8
promBits Coeff5 = 10
promBits Coeff6 = 12
