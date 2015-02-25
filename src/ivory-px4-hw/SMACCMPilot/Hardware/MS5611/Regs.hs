{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.MS5611.Regs where

import Ivory.Language

data Command
  = Reset
  | ConvertD1 OSR
  | ConvertD2 OSR
  | ADCRead
  | PromRead PROM

data OSR
  = OSR256
  | OSR512
  | OSR1024
  | OSR2048
  | OSR4096
  deriving (Eq, Show)

data PROM
  = Reserved
  | Coeff (Ix 6)

commandVal :: Command -> Uint8
commandVal Reset           = 0x1E
commandVal (ConvertD1 osr) = 0x40 .| osrBits osr
commandVal (ConvertD2 osr) = 0x50 .| osrBits osr
commandVal ADCRead         = 0x00
commandVal (PromRead prom) = 0xA0 .| promBits prom

osrBits :: OSR -> Uint8
osrBits OSR256  = 0
osrBits OSR512  = 2
osrBits OSR1024 = 4
osrBits OSR2048 = 6
osrBits OSR4096 = 8

promBits :: PROM -> Uint8
promBits Reserved  = 0
promBits (Coeff i) = castDefault (fromIx i + 1) * 2
