{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Hardware.Sensors where

import Ivory.Language

import           Ivory.BSP.STM32.Peripheral.UART
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
import qualified Ivory.BSP.STM32F427.SPI            as F427

data Sensors
  = FMU17Sensors
    { fmu17sens_mpu6000    :: SPIDevice
    , fmu17sens_spi_pins   :: SPIPins
    , fmu17sens_spi_periph :: SPIPeriph
    , fmu17sens_ms5611     :: I2CDeviceAddr
    , fmu17sens_hmc5883l   :: I2CDeviceAddr
    , fmu17sens_i2c_periph :: I2CPeriph
    , fmu17sens_i2c_pins   :: I2CPins
    }
  | FMU24Sensors
    { fmu24sens_mpu6000    :: SPIDevice
    , fmu24sens_ms5611     :: SPIDevice
    , fmu24sens_lsm303d    :: SPIDevice
    , fmu24sens_l3gd20     :: SPIDevice
    , fmu24sens_spi_periph :: SPIPeriph
    , fmu24sens_spi_pins   :: SPIPins
    , fmu24sens_enable     :: forall eff . Ivory eff ()
    }

data UART_Device =
  UART_Device
    { uart_periph :: UART
    , uart_pins   :: UARTPins
    }

-----

fmu17_sensors :: Sensors
fmu17_sensors = FMU17Sensors
  { fmu17sens_mpu6000    = mpu6000
  , fmu17sens_spi_pins   = spi1_pins
  , fmu17sens_spi_periph = spi1_periph
  , fmu17sens_ms5611     = I2CDeviceAddr 0x76
  , fmu17sens_hmc5883l   = I2CDeviceAddr 0x1e
  , fmu17sens_i2c_periph = F405.i2c2
  , fmu17sens_i2c_pins   = i2c2_pins
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

fmu24_sensors :: Sensors
fmu24_sensors = FMU24Sensors
  { fmu24sens_mpu6000    = mpu6000
  , fmu24sens_ms5611     = ms5611
  , fmu24sens_lsm303d    = lsm303d
  , fmu24sens_l3gd20     = l3gd20
  , fmu24sens_spi_pins   = spi1_pins
  , fmu24sens_spi_periph = spi1_periph
  , fmu24sens_enable     = sensor_enable
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
