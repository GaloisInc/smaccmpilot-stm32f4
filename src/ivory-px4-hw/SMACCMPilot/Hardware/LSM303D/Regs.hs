{-# LANGUAGE RecordWildCards #-}

module SMACCMPilot.Hardware.LSM303D.Regs where

import Data.Word
import Data.Bits

-- This is a partial register map, there are a ton
-- of registers we just don't care about.
data Reg
  = R_Magic1
  | R_TempOutL
  | R_TempOutH
  | R_StatusM
  | R_OutXLM
  | R_OutXHM
  | R_OutYLM
  | R_OutYHM
  | R_OutZLM
  | R_OutZHM
  | R_WhoAmI
  | R_Magic2
  | R_Control0
  | R_Control1
  | R_Control2
  | R_Control3
  | R_Control4
  | R_Control5
  | R_Control6
  | R_Control7
  | R_StatusA
  | R_OutXLA
  | R_OutXHA
  | R_OutYLA
  | R_OutYHA
  | R_OutZLA
  | R_OutZHA
  deriving (Eq, Show)

data Config =
  Config
    { conf_ctl1 :: Control1
    , conf_ctl2 :: Control2
    , conf_ctl5 :: Control5
    , conf_ctl6 :: Control6
    , conf_ctl7 :: Control7
    }

data Control1 =
  Control1
      { accel_datarate    :: AccelDatarate
      , block_data_update :: Bool
      , accel_x_enable    :: Bool
      , accel_y_enable    :: Bool
      , accel_z_enable    :: Bool
      }

data Control2 =
  Control2
      { accel_anti_alias_bw  :: AccelAntiAliasBandwidth
      , accel_full_scale     :: AccelFullScale
      , accel_self_test      :: Bool
      , spi_3wire_mode :: Bool
      }

data Control5 =
  Control5
      { temp_enable :: Bool
      , mag_resolution :: MagResolution
      , mag_datarate   :: MagDatarate
      } -- remaining fields leave zero

data Control6 =
  Control6
      { mag_full_scale :: MagFullScale
      }

data Control7 =
  Control7
      { mag_power_mode       :: MagPowerMode
      } -- remaining fields leave zero

data AccelDatarate
  = ADR_PowerDown
  | ADR_3_125hz
  | ADR_6_25hz
  | ADR_12_5hz
  | ADR_25hz
  | ADR_50hz
  | ADR_100hz
  | ADR_200hz
  | ADR_400hz
  | ADR_800hz
  | ADR_1600hz

data AccelAntiAliasBandwidth
  = AABW_733hz
  | AABW_194hz
  | AABW_362hz
  | AABW_50hz

data AccelFullScale
  = AAFS_2g
  | AAFS_4g
  | AAFS_6g
  | AAFS_8g
  | AAFS_16g

data MagResolution
  = MR_Low
  | MR_High

data MagDatarate
  = MDR_3_125hz
  | MDR_6_25hz
  | MDR_12_5hz
  | MDR_25hz
  | MDR_50hz
  | MDR_100hz

data MagFullScale
  = MFS_2gauss
  | MFS_4gauss
  | MFS_8gauss
  | MFS_12gauss

data MagPowerMode
  = MPM_Continuous
  | MPM_Single
  | MPM_PowerDown

regAddr :: Reg -> Word8
regAddr R_Magic1 = 0x02
regAddr R_TempOutL = 0x05
regAddr R_TempOutH = 0x06
regAddr R_StatusM = 0x07
regAddr R_OutXLM = 0x08
regAddr R_OutXHM = 0x09
regAddr R_OutYLM = 0x0a
regAddr R_OutYHM = 0x0b
regAddr R_OutZLM = 0x0c
regAddr R_OutZHM = 0x0d
regAddr R_WhoAmI = 0x0f
regAddr R_Magic2 =  0x15
regAddr R_Control0 = 0x1f
regAddr R_Control1 = 0x20
regAddr R_Control2 = 0x21
regAddr R_Control3 = 0x22
regAddr R_Control4 = 0x23
regAddr R_Control5 = 0x24
regAddr R_Control6 = 0x25
regAddr R_Control7 = 0x26
regAddr R_StatusA = 0x27
regAddr R_OutXLA = 0x28
regAddr R_OutXHA = 0x29
regAddr R_OutYLA = 0x2a
regAddr R_OutYHA = 0x2b
regAddr R_OutZLA = 0x2c
regAddr R_OutZHA = 0x2d

readReg :: Reg -> Word8
readReg r = regAddr r .|. bit 7

readSequentialRegs :: Reg -> Word8
readSequentialRegs r = readReg r .|. bit 6

writeReg :: Reg -> Word8
writeReg r = regAddr r

accelDatarateVal :: AccelDatarate -> Word8
accelDatarateVal ADR_PowerDown = 0
accelDatarateVal ADR_3_125hz   = 1
accelDatarateVal ADR_6_25hz    = 2
accelDatarateVal ADR_12_5hz    = 3
accelDatarateVal ADR_25hz      = 4
accelDatarateVal ADR_50hz      = 5
accelDatarateVal ADR_100hz     = 6
accelDatarateVal ADR_200hz     = 7
accelDatarateVal ADR_400hz     = 8
accelDatarateVal ADR_800hz     = 9
accelDatarateVal ADR_1600hz    = 10

accelAntiAliasBandwidthVal :: AccelAntiAliasBandwidth -> Word8
accelAntiAliasBandwidthVal AABW_733hz = 0
accelAntiAliasBandwidthVal AABW_194hz = 1
accelAntiAliasBandwidthVal AABW_362hz = 2
accelAntiAliasBandwidthVal AABW_50hz  = 3

accelFullScaleVal :: AccelFullScale -> Word8
accelFullScaleVal AAFS_2g  = 0
accelFullScaleVal AAFS_4g  = 1
accelFullScaleVal AAFS_6g  = 2
accelFullScaleVal AAFS_8g  = 3
accelFullScaleVal AAFS_16g = 4

magResolutionVal :: MagResolution -> Word8
magResolutionVal MR_Low  = 0
magResolutionVal MR_High = 3

magDatarateVal :: MagDatarate -> Word8
magDatarateVal MDR_3_125hz = 0
magDatarateVal MDR_6_25hz  = 1
magDatarateVal MDR_12_5hz  = 2
magDatarateVal MDR_25hz    = 3
magDatarateVal MDR_50hz    = 4
magDatarateVal MDR_100hz   = 5

magFullScaleVal :: MagFullScale -> Word8
magFullScaleVal MFS_2gauss  = 0
magFullScaleVal MFS_4gauss  = 1
magFullScaleVal MFS_8gauss  = 2
magFullScaleVal MFS_12gauss = 3

magPowerModeVal :: MagPowerMode -> Word8
magPowerModeVal MPM_Continuous = 0
magPowerModeVal MPM_Single     = 1
magPowerModeVal MPM_PowerDown  = 2

bField :: Bool -> Int -> Word8
bField False _ = 0
bField True n = bit n

wField :: Word8 -> Int -> Word8
wField v n = shiftL v n

control1Val :: Control1 -> Word8
control1Val (Control1{..}) =
      wField (accelDatarateVal accel_datarate) 4
  .|. bField block_data_update 3
  .|. bField accel_x_enable    2
  .|. bField accel_y_enable    1
  .|. bField accel_z_enable    0

control2Val :: Control2 -> Word8
control2Val (Control2{..}) =
      wField (accelAntiAliasBandwidthVal accel_anti_alias_bw) 6
  .|. wField (accelFullScaleVal accel_full_scale)             3
  .|. bField accel_self_test                                  1
  .|. bField spi_3wire_mode                                   0

control5Val :: Control5 -> Word8
control5Val (Control5{..}) =
      bField temp_enable 7
  .|. wField (magResolutionVal mag_resolution) 5
  .|. wField (magDatarateVal mag_datarate) 2

control6Val :: Control6 -> Word8
control6Val (Control6{..}) =
      wField (magFullScaleVal mag_full_scale) 5

control7Val :: Control7 -> Word8
control7Val (Control7{..}) =
      wField (magPowerModeVal mag_power_mode) 0

magSensitivityGauss :: Fractional a => Config -> a
magSensitivityGauss c = 1.0e-3 * case mag_full_scale (conf_ctl6 c) of
  -- milli-gauss/LSB
  MFS_2gauss  -> 0.080
  MFS_4gauss  -> 0.160
  MFS_8gauss  -> 0.320
  MFS_12gauss -> 0.479

accelSensitivityMSS :: Fractional a => Config -> a
accelSensitivityMSS c = 9.80665e-3 * case accel_full_scale (conf_ctl2 c) of
  -- milli-g/LSB
  AAFS_2g  -> 0.061
  AAFS_4g  -> 0.122
  AAFS_6g  -> 0.183
  AAFS_8g  -> 0.244
  AAFS_16g -> 0.732

