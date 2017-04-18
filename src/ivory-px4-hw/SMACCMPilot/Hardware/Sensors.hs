{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}

module SMACCMPilot.Hardware.Sensors where

import Prelude ()
import Prelude.Compat

import Linear

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Config
import Ivory.Tower.HAL.Bus.Interface

import           Ivory.BSP.STM32.Peripheral.UART
import           Ivory.BSP.STM32.Peripheral.UART.DMA
import           Ivory.BSP.STM32.Peripheral.GPIOF4
import           Ivory.BSP.STM32.Peripheral.SPI
import           Ivory.BSP.STM32.Peripheral.I2C
import           Ivory.BSP.STM32.Driver.I2C

import qualified Ivory.BSP.STM32F405.GPIO           as F405
import qualified Ivory.BSP.STM32F405.GPIO.AF        as F405
import qualified Ivory.BSP.STM32F405.SPI            as F405
import qualified Ivory.BSP.STM32F405.I2C            as F405

import qualified Ivory.BSP.STM32F427.GPIO           as F427
import qualified Ivory.BSP.STM32F427.GPIO.AF        as F427
import qualified Ivory.BSP.STM32F427.I2C            as F427
import qualified Ivory.BSP.STM32F427.SPI            as F427

data Sensors
  = FMU17Sensors
    { fmu17sens_mpu6000           :: SPIDevice
    , fmu17sens_mpu6000_accel_cal :: AccelCal
    , fmu17sens_mpu6000_gyro_cal  :: GyroCal
    , fmu17sens_spi_pins          :: SPIPins
    , fmu17sens_spi_periph        :: SPIPeriph
    , fmu17sens_ms5611            :: I2CDeviceAddr
    , fmu17sens_hmc5883l          :: I2CDeviceAddr
    , fmu17sens_hmc5883l_mag_cal  :: MagCal
    , fmu17sens_local_mag         :: LocalMag
    , fmu17sens_i2c_periph        :: I2CPeriph
    , fmu17sens_i2c_pins          :: I2CPins
    }
  | FMU24Sensors
    { fmu24sens_mpu6000           :: SPIDevice
    , fmu24sens_mpu6000_accel_cal :: AccelCal
    , fmu24sens_mpu6000_gyro_cal  :: GyroCal
    , fmu24sens_ms5611            :: SPIDevice
    , fmu24sens_lsm303d           :: SPIDevice
    , fmu24sens_lsm303d_mag_cal   :: MagCal
    , fmu24sens_local_mag         :: LocalMag
    , fmu24sens_lsm303d_accel_cal :: AccelCal
    , fmu24sens_l3gd20            :: SPIDevice
    , fmu24sens_spi_periph        :: SPIPeriph
    , fmu24sens_spi_pins          :: SPIPins
    , fmu24sens_enable            :: forall eff . Ivory eff ()
    , fmu24sens_ext_i2c_periph    :: I2CPeriph
    , fmu24sens_ext_i2c_pins      :: I2CPins
    }

data UART_Device =
  UART_Device
    { uart_periph :: Either UART DMAUART
    , uart_pins   :: UARTPins
    }

data ExternalSensor req res = ExternalSensor
  { ext_sens_name :: String
  , ext_sens_init
      :: forall e . BackpressureTransmit req res
      -> ChanOutput ('Stored ITime)
      -> Tower e ()
  }

type ExternalI2CSensor =
  ExternalSensor ('Struct "i2c_transaction_request")
                 ('Struct "i2c_transaction_result")

data XyzCal = XyzCal {
    cal_x_offset :: Sint16
  , cal_y_offset :: Sint16
  , cal_z_offset :: Sint16
  , cal_x_scale :: IFloat
  , cal_y_scale :: IFloat
  , cal_z_scale :: IFloat
  }

parseXyzCal :: ConfigParser XyzCal
parseXyzCal = do
  cal_x_offset <- fromInteger <$> subsection "x_offset" integer
  cal_y_offset <- fromInteger <$> subsection "y_offset" integer
  cal_z_offset <- fromInteger <$> subsection "z_offset" integer
  cal_x_scale <- toIFloat <$> subsection "x_scale" double
  cal_y_scale <- toIFloat <$> subsection "y_scale" double
  cal_z_scale <- toIFloat <$> subsection "z_scale" double
  return $ XyzCal {..}

toIFloat :: Double -> IFloat
toIFloat = fromRational . toRational

applyXyzCal
  :: XyzCal -> (Sint16 -> IFloat, Sint16 -> IFloat, Sint16 -> IFloat)
applyXyzCal XyzCal {..} = (cal_x, cal_y, cal_z)
  where
    cal_x x = safeCast (x - cal_x_offset) * cal_x_scale
    cal_y y = safeCast (y - cal_y_offset) * cal_y_scale
    cal_z z = safeCast (z - cal_z_offset) * cal_z_scale

newtype AccelCal = AccelCal XyzCal

parseAccelCal :: String -> ConfigParser AccelCal
parseAccelCal periph
  = subsection "calibration"
  $ subsection periph
  $ subsection "accelerometer"
  $ fmap AccelCal parseXyzCal

newtype GyroCal = GyroCal XyzCal

parseGyroCal :: String -> ConfigParser GyroCal
parseGyroCal periph
  = subsection "calibration"
  $ subsection periph
  $ subsection "gyroscope"
  $ fmap GyroCal parseXyzCal

newtype MagCal = MagCal XyzCal

parseMagCal :: String -> ConfigParser MagCal
parseMagCal periph
  = subsection "calibration"
  $ subsection periph
  $ subsection "magnetometer"
  $ fmap MagCal parseXyzCal

type LocalMag = V3 IFloat

parseLocalMag :: ConfigParser LocalMag
parseLocalMag
  = subsection "calibration"
  $ subsection "magnetometer"
  $ subsection "local_field" $ do
    x <- toIFloat <$> subsection "x" double
    y <- toIFloat <$> subsection "y" double
    z <- toIFloat <$> subsection "z" double
    return (signorm (V3 x y z))

sensors_local_mag :: Sensors -> LocalMag
sensors_local_mag sens = case sens of
  FMU17Sensors{..} -> fmu17sens_local_mag
  FMU24Sensors{..} -> fmu24sens_local_mag

-----

fmu17_sensors :: ConfigParser Sensors
fmu17_sensors = do
  mpu6000_accel_cal <- parseAccelCal "mpu6000"
  mpu6000_gyro_cal <- parseGyroCal "mpu6000"
  hmc5883l_mag_cal <- parseMagCal "hmc5883l"
  local_mag <- parseLocalMag
  return $ FMU17Sensors
    { fmu17sens_mpu6000           = mpu6000
    , fmu17sens_mpu6000_accel_cal = mpu6000_accel_cal
    , fmu17sens_mpu6000_gyro_cal  = mpu6000_gyro_cal
    , fmu17sens_spi_pins          = spi1_pins
    , fmu17sens_spi_periph        = spi1_periph
    , fmu17sens_ms5611            = I2CDeviceAddr 0x76
    , fmu17sens_hmc5883l          = I2CDeviceAddr 0x1e
    , fmu17sens_hmc5883l_mag_cal  = hmc5883l_mag_cal
    , fmu17sens_local_mag         = local_mag
    , fmu17sens_i2c_periph        = F405.i2c2
    , fmu17sens_i2c_pins          = i2c2_pins
    }
  where
  mpu6000 :: SPIDevice
  mpu6000 = SPIDevice
    { spiDevPeripheral    = spi1_periph
    , spiDevCSPin         = F405.pinB0
    , spiDevClockHz       = 500000
    , spiDevCSActive      = ActiveLow
    , spiDevClockPolarity = ClockPolarityLow
    , spiDevClockPhase    = ClockPhase1
    , spiDevBitOrder      = MSBFirst
    , spiDevName          = "mpu6k"
    }
  spi1_periph :: SPIPeriph
  spi1_periph = F405.spi1
  spi1_pins :: SPIPins
  spi1_pins = SPIPins
    { spiPinMiso = F405.pinA7
    , spiPinMosi = F405.pinA6
    , spiPinSck  = F405.pinA5
    , spiPinAF   = F405.gpio_af_spi1
    }
  i2c2_pins :: I2CPins
  i2c2_pins = I2CPins
    { i2cpins_sda = F405.pinB10
    , i2cpins_scl = F405.pinB11
    }

fmu24_sensors :: ConfigParser Sensors
fmu24_sensors = do
  mpu6000_accel_cal <- parseAccelCal "mpu6000"
  mpu6000_gyro_cal <- parseGyroCal "mpu6000"
  lsm303d_mag_cal <- parseMagCal "lsm303d"
  local_mag <- parseLocalMag
  lsm303d_accel_cal <- parseAccelCal "lsm303d"
  return $ FMU24Sensors
    { fmu24sens_mpu6000           = mpu6000
    , fmu24sens_mpu6000_accel_cal = mpu6000_accel_cal
    , fmu24sens_mpu6000_gyro_cal  = mpu6000_gyro_cal
    , fmu24sens_ms5611            = ms5611
    , fmu24sens_lsm303d           = lsm303d
    , fmu24sens_lsm303d_mag_cal   = lsm303d_mag_cal
    , fmu24sens_local_mag         = local_mag
    , fmu24sens_lsm303d_accel_cal = lsm303d_accel_cal
    , fmu24sens_l3gd20            = l3gd20
    , fmu24sens_spi_pins          = spi1_pins
    , fmu24sens_spi_periph        = spi1_periph
    , fmu24sens_ext_i2c_pins      = i2c1_pins
    , fmu24sens_ext_i2c_periph    = i2c1_periph
    , fmu24sens_enable            = sensor_enable
    }
  where
  mpu6000 :: SPIDevice
  mpu6000 = SPIDevice
    { spiDevPeripheral    = spi1_periph
    , spiDevCSPin         = F427.pinC2
    , spiDevClockHz       = 500000
    , spiDevCSActive      = ActiveLow
    , spiDevClockPolarity = ClockPolarityHigh
    , spiDevClockPhase    = ClockPhase2
    , spiDevBitOrder      = MSBFirst
    , spiDevName          = "mpu6k"
    }
  ms5611 :: SPIDevice
  ms5611 = SPIDevice
    { spiDevPeripheral    = spi1_periph
    , spiDevCSPin         = F427.pinD7
    , spiDevClockHz       = 500000
    , spiDevCSActive      = ActiveLow
    , spiDevClockPolarity = ClockPolarityHigh
    , spiDevClockPhase    = ClockPhase2
    , spiDevBitOrder      = MSBFirst
    , spiDevName          = "ms5611"
    }
  lsm303d :: SPIDevice
  lsm303d = SPIDevice
    { spiDevPeripheral    = spi1_periph
    , spiDevCSPin         = F427.pinC15
    , spiDevClockHz       = 500000
    , spiDevCSActive      = ActiveLow
    , spiDevClockPolarity = ClockPolarityHigh
    , spiDevClockPhase    = ClockPhase2
    , spiDevBitOrder      = MSBFirst
    , spiDevName          = "lsm303d"
    }
  l3gd20 :: SPIDevice
  l3gd20 = SPIDevice
    { spiDevPeripheral    = spi1_periph
    , spiDevCSPin         = F427.pinC13
    , spiDevClockHz       = 500000
    , spiDevCSActive      = ActiveLow
    , spiDevClockPolarity = ClockPolarityHigh
    , spiDevClockPhase    = ClockPhase2
    , spiDevBitOrder      = MSBFirst
    , spiDevName          = "l3gd20"
    }
  spi1_periph :: SPIPeriph
  spi1_periph = F427.spi1
  spi1_pins :: SPIPins
  spi1_pins = SPIPins
    { spiPinMiso = F427.pinA6
    , spiPinMosi = F427.pinA7
    , spiPinSck  = F427.pinA5
    , spiPinAF   = F427.gpio_af_spi1
    }
  i2c1_periph :: I2CPeriph
  i2c1_periph = F427.i2c1
  i2c1_pins :: I2CPins
  i2c1_pins = I2CPins
    { i2cpins_sda = F427.pinB9
    , i2cpins_scl = F427.pinB8
    }

  sensor_enable :: Ivory eff ()
  sensor_enable = do
    -- Turn on sensor vdd regulator
    comment "Pixhawk VDD 3V3 Sensors Enable"
    assert_gpio F427.pinE3 -- VDD 3V3 Sensors Enable
    comment "Pixhawk VDD 5V Periph Enable"
    clear_gpio F427.pinA8 -- VDD 5 Periph Enable
    -- Ensure all CS pins are deselected, so that each device MISO lines are High-Z
    comment "assert Gyro CS"
    assert_gpio F427.pinC13 -- Gyro CS
    comment "assert Accel Mag CS"
    assert_gpio F427.pinC15 -- Accel Mag CS
    comment "assert MPU6000 CS"
    assert_gpio F427.pinC2  -- MPU6k CS
    comment "assert Baro CS"
    assert_gpio F427.pinD7  -- Baro CS
    comment "assert FRAM CS"
    assert_gpio F427.pinD10 -- FRAM CS

  output_gpio :: GPIOPin -> Ivory eff ()
  output_gpio p = do
    pinEnable        p
    pinSetMode       p gpio_mode_output
    pinSetOutputType p gpio_outputtype_pushpull
    pinSetSpeed      p gpio_speed_50mhz

  assert_gpio :: GPIOPin -> Ivory eff ()
  assert_gpio p = do
    comment ("Set " ++ pinName p ++ " to assert output")
    output_gpio      p
    pinSet           p

  clear_gpio :: GPIOPin -> Ivory eff ()
  clear_gpio p = do
    comment ("Set " ++ pinName p ++ " to clear output")
    output_gpio      p
    pinClear         p
