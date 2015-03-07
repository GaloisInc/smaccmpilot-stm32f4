{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PX4.Tests.Platforms where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Config

import Data.Char (toUpper)

import qualified SMACCMPilot.Hardware.PX4FMU17 as FMUv17

import qualified Ivory.BSP.STM32F405.UART           as F405
import qualified Ivory.BSP.STM32F405.GPIO           as F405
import qualified Ivory.BSP.STM32F405.GPIO.AF        as F405
import qualified Ivory.BSP.STM32F405.SPI            as F405
import qualified Ivory.BSP.STM32F405.I2C            as F405
import qualified Ivory.BSP.STM32F405.ATIM18         as F405
import qualified Ivory.BSP.STM32F405.Interrupt      as F405
import qualified Ivory.BSP.STM32F427.UART           as F427
import qualified Ivory.BSP.STM32F427.GPIO           as F427
import qualified Ivory.BSP.STM32F427.GPIO.AF        as F427
import qualified Ivory.BSP.STM32F427.SPI            as F427
import           Ivory.BSP.STM32.Peripheral.GPIOF4
import           Ivory.BSP.STM32.Peripheral.UART
import           Ivory.BSP.STM32.Peripheral.SPI
import           Ivory.BSP.STM32.Peripheral.I2C
import           Ivory.BSP.STM32.Peripheral.ATIM18
import           Ivory.BSP.STM32.Interrupt
import           Ivory.BSP.STM32.Driver.I2C
import           Ivory.BSP.STM32.Driver.UART
import           Ivory.BSP.STM32.ClockConfig
import           Ivory.OS.FreeRTOS.Tower.STM32.Config

data PX4Platform =
  PX4Platform
    { px4platform_gps            :: UART_Device
    , px4platform_mpu6000        :: MPU6000_SPI
    , px4platform_baro           :: Baro
    , px4platform_mag            :: Magnetometer
    , px4platform_sensorenable   :: forall eff . Ivory eff ()

    , px4platform_motorcontrol   :: forall e . (e -> ClockConfig)
                                 -> ChanOutput (Array 4 (Stored IFloat))
                                 -> Tower e ()
    , px4platform_ppm            :: PPM

    , px4platform_console        :: UART_Device

    , px4platform_stm32config    :: STM32Config
    }

data UART_Device =
  UART_Device
    { uart_periph :: UART
    , uart_pins   :: UARTPins
    }

data MPU6000_SPI =
  MPU6000_SPI
    { mpu6000_spi_device :: SPIDevice
    , mpu6000_spi_pins   :: SPIPins
    }

data Baro
  = Baro_MS5611_I2C MS5611_I2C
  | Baro_MS5611_SPI MS5611_SPI

data MS5611_I2C =
  MS5611_I2C
    { ms5611_i2c_periph :: I2CPeriph
    , ms5611_i2c_sda    :: GPIOPin
    , ms5611_i2c_scl    :: GPIOPin
    , ms5611_i2c_addr   :: I2CDeviceAddr
    }

data MS5611_SPI =
  MS5611_SPI
    { ms5611_spi_device :: SPIDevice
    , ms5611_spi_pins   :: SPIPins
    -- Invariant: Pins are the same as MPU6000, by fiat
    }

data Magnetometer
  = Mag_HMC5883L_I2C HMC5883L_I2C
  | Mag_LSM303D_SPI  LSM303D_SPI

data HMC5883L_I2C =
  HMC5883L_I2C
    { hmc5883l_i2c_periph :: I2CPeriph
    , hmc5883l_i2c_sda    :: GPIOPin
    , hmc5883l_i2c_scl    :: GPIOPin
    , hmc5883l_i2c_addr   :: I2CDeviceAddr
    }

data LSM303D_SPI =
  LSM303D_SPI
    { lsm303d_spi_device :: SPIDevice
    , lsm303d_spi_pins   :: SPIPins
    -- Invariant: Pins are the same as MPU6000, by fiat
    }

data PPM
  = PPM_Timer ATIM GPIOPin GPIO_AF HasSTM32Interrupt
  | PPM_None




-----------------


px4PlatformParser :: ConfigParser PX4Platform
px4PlatformParser = do
  p <- subsection "args" $ subsection "platform" string
  case map toUpper p of
    "PX4FMUV17"      -> return px4fmuv17
    "PX4FMUV17_IOAR" -> return px4fmuv17_ioar
    "PX4FMUV24"      -> return px4fmuv24
    "PIXHAWK"        -> return px4fmuv24
    "ESB_X1"         -> return esb_x1
    _ -> fail ("no such platform " ++ p)

px4fmuv17 :: PX4Platform
px4fmuv17 = PX4Platform
  { px4platform_gps          = gps
  , px4platform_mpu6000      = mpu6000
  , px4platform_mag          = Mag_HMC5883L_I2C hmc5883l
  , px4platform_baro         = Baro_MS5611_I2C  ms5611
  , px4platform_sensorenable = return ()
  , px4platform_motorcontrol = FMUv17.motorControlTower
  , px4platform_ppm          = ppm_timer
  , px4platform_console      = console
  , px4platform_stm32config  = stm32f405Defaults 24
  }
  where
  console :: UART_Device
  console = UART_Device
    { uart_periph = F405.uart5
    , uart_pins = UARTPins
        { uartPinTx = F405.pinC12
        , uartPinRx = F405.pinD2
        , uartPinAF = F405.gpio_af_uart5
        }
    }
  gps :: UART_Device
  gps = UART_Device
    { uart_periph = F405.uart6
    , uart_pins   = UARTPins
      { uartPinTx = F405.pinC6
      , uartPinRx = F405.pinC7
      , uartPinAF = F405.gpio_af_uart6
      }
    }
  mpu6000 :: MPU6000_SPI
  mpu6000 = MPU6000_SPI
    { mpu6000_spi_device = SPIDevice
      { spiDevPeripheral    = F405.spi1
      , spiDevCSPin         = F405.pinB0
      , spiDevClockHz       = 500000
      , spiDevCSActive      = ActiveLow
      , spiDevClockPolarity = ClockPolarityLow
      , spiDevClockPhase    = ClockPhase1
      , spiDevBitOrder      = MSBFirst
      , spiDevName          = "mpu6k"
      }
    , mpu6000_spi_pins = spi1_pins
    }
  spi1_pins :: SPIPins
  spi1_pins = SPIPins
    { spiPinMiso = F405.pinA7
    , spiPinMosi = F405.pinA6
    , spiPinSck  = F405.pinA5
    , spiPinAF   = F405.gpio_af_spi1
    }
  hmc5883l :: HMC5883L_I2C
  hmc5883l = HMC5883L_I2C
    { hmc5883l_i2c_periph = F405.i2c2
    , hmc5883l_i2c_sda    = F405.pinB10
    , hmc5883l_i2c_scl    = F405.pinB11
    , hmc5883l_i2c_addr   = I2CDeviceAddr 0x1e
    }
  ms5611 :: MS5611_I2C
  ms5611 = MS5611_I2C
    { ms5611_i2c_periph = F405.i2c2
    , ms5611_i2c_sda    = F405.pinB10
    , ms5611_i2c_scl    = F405.pinB11
    , ms5611_i2c_addr   = I2CDeviceAddr 0x76
    }
  ppm_timer :: PPM
  ppm_timer = PPM_Timer F405.tim1 F405.pinA10 F405.gpio_af_tim1 ppm_int
  ppm_int = HasSTM32Interrupt F405.TIM1_CC

px4fmuv17_ioar :: PX4Platform
px4fmuv17_ioar = px4fmuv17 { px4platform_console = console }
  where
  console :: UART_Device
  console = UART_Device
    { uart_periph = F405.uart1
    , uart_pins   = UARTPins
        { uartPinTx = F405.pinB6
        , uartPinRx = F405.pinB7
        , uartPinAF = F405.gpio_af_uart1
        }
    }

px4fmuv24 :: PX4Platform
px4fmuv24 = PX4Platform
  { px4platform_gps          = gps
  , px4platform_mpu6000      = mpu6000
  , px4platform_mag          = Mag_LSM303D_SPI lsm303d
  , px4platform_baro         = Baro_MS5611_SPI ms5611
  , px4platform_sensorenable = sensor_enable
  , px4platform_motorcontrol = error "motor control not defined for px4fmuv24"
  , px4platform_ppm          = PPM_None -- XXX need px4io driver.
  , px4platform_console      = console
  , px4platform_stm32config  = stm32f427Defaults 24
  }
  where
  console = UART_Device -- Telem 1 Port
    { uart_periph = F427.uart2
    , uart_pins = UARTPins
        { uartPinTx = F427.pinD5
        , uartPinRx = F427.pinD6
        , uartPinAF = F427.gpio_af_uart2
        }
    }
  gps = UART_Device
    { uart_periph = F427.uart4
    , uart_pins = UARTPins
        { uartPinTx = F427.pinA0
        , uartPinRx = F427.pinA1
        , uartPinAF = F427.gpio_af_uart4
        }
    }
  mpu6000 = MPU6000_SPI
    { mpu6000_spi_device = SPIDevice
      { spiDevPeripheral    = F427.spi1
      , spiDevCSPin         = F427.pinC2
      , spiDevClockHz       = 500000
      , spiDevCSActive      = ActiveLow
      , spiDevClockPolarity = ClockPolarityLow
      , spiDevClockPhase    = ClockPhase1
      , spiDevBitOrder      = MSBFirst
      , spiDevName          = "mpu6k"
      }
    , mpu6000_spi_pins = spi1_pins
    }
  spi1_pins :: SPIPins
  spi1_pins = SPIPins
    { spiPinMiso = F427.pinA6
    , spiPinMosi = F427.pinA7
    , spiPinSck  = F427.pinA5
    , spiPinAF   = F427.gpio_af_spi1
    }
  ms5611 :: MS5611_SPI
  ms5611 = MS5611_SPI
    { ms5611_spi_device = SPIDevice
      { spiDevPeripheral    = F427.spi1
      , spiDevCSPin         = F427.pinD7
      , spiDevClockHz       = 500000
      , spiDevCSActive      = ActiveLow
      , spiDevClockPolarity = ClockPolarityLow
      , spiDevClockPhase    = ClockPhase1
      , spiDevBitOrder      = MSBFirst
      , spiDevName          = "ms5611"
      }
    , ms5611_spi_pins = spi1_pins
    }
  lsm303d :: LSM303D_SPI
  lsm303d = LSM303D_SPI
    { lsm303d_spi_device = SPIDevice
      { spiDevPeripheral    = F427.spi1
      , spiDevCSPin         = F427.pinC15
      , spiDevClockHz       = 500000
      , spiDevCSActive      = ActiveLow
      , spiDevClockPolarity = ClockPolarityLow
      , spiDevClockPhase    = ClockPhase1
      , spiDevBitOrder      = MSBFirst
      , spiDevName          = "lsm303d"
      }
    , lsm303d_spi_pins = spi1_pins
    }
  sensor_enable = do
    -- Turn on sensor vdd regulator
    assert_gpio F427.pinE3 -- VDD Enable
    -- Ensure all CS pins are deselected, so that MISO lines are all High-Z
    assert_gpio F427.pinC13 -- Gyro CS
    assert_gpio F427.pinC15 -- Accel Mag CS
    assert_gpio F427.pinC2  -- MPU6k CS
    assert_gpio F427.pinD7  -- Baro CS

  assert_gpio :: GPIOPin -> Ivory eff ()
  assert_gpio p = do
    pinEnable        p
    pinSetMode       p gpio_mode_output
    pinSetOutputType p gpio_outputtype_pushpull
    pinSetSpeed      p gpio_speed_50mhz
    pinSet           p


----

esb_x1 :: PX4Platform
esb_x1 = PX4Platform
  { px4platform_gps          = gps
  , px4platform_mpu6000      = mpu6000
  , px4platform_mag          = error "magnetometer not defined for esb_x1"
  , px4platform_baro         = Baro_MS5611_SPI ms5611
  , px4platform_sensorenable = sensor_enable
  , px4platform_motorcontrol = error "motor control not defined for esb_x1"
  , px4platform_ppm          = PPM_None -- XXX need px4io driver.
  , px4platform_console      = console
  , px4platform_stm32config  = stm32f427Defaults 24
  }
  where
  console = UART_Device -- Telem 1 Port
    { uart_periph = F427.uart2
    , uart_pins = UARTPins
        { uartPinTx = F427.pinD5
        , uartPinRx = F427.pinD6
        , uartPinAF = F427.gpio_af_uart2
        }
    }
  gps = UART_Device
    { uart_periph = F427.uart3 -- Telem 2 port
    , uart_pins = UARTPins
        { uartPinTx = F427.pinD8
        , uartPinRx = F427.pinD9
        , uartPinAF = F427.gpio_af_uart3
        }
    }
  mpu6000 = MPU6000_SPI
    { mpu6000_spi_device = SPIDevice
      { spiDevPeripheral    = F427.spi3
      , spiDevCSPin         = F427.pinC2
      , spiDevClockHz       = 500000
      , spiDevCSActive      = ActiveLow
      , spiDevClockPolarity = ClockPolarityLow
      , spiDevClockPhase    = ClockPhase1
      , spiDevBitOrder      = MSBFirst
      , spiDevName          = "mpu6k"
      }
    , mpu6000_spi_pins = spi3_pins
    }
  spi3_pins :: SPIPins
  spi3_pins = SPIPins
    { spiPinMiso = F427.pinC11
    , spiPinMosi = F427.pinC12
    , spiPinSck  = F427.pinC10
    , spiPinAF   = gpio_af6
    }
  ms5611 :: MS5611_SPI
  ms5611 = MS5611_SPI
    { ms5611_spi_device = SPIDevice
      { spiDevPeripheral    = F427.spi3
      , spiDevCSPin         = F427.pinC1
      , spiDevClockHz       = 500000
      , spiDevCSActive      = ActiveLow
      , spiDevClockPolarity = ClockPolarityLow
      , spiDevClockPhase    = ClockPhase1
      , spiDevBitOrder      = MSBFirst
      , spiDevName          = "ms5611"
      }
    , ms5611_spi_pins = spi3_pins
    }
  sensor_enable = do
    -- Make sure all CS pins are deselected, so that MISO lines are all High-Z
    assert_gpio F427.pinC1 -- baro cs
    assert_gpio F427.pinC2 -- mpu6k cs
    assert_gpio F427.pinC3 -- thermo cs

  assert_gpio :: GPIOPin -> Ivory eff ()
  assert_gpio p = do
    pinEnable        p
    pinSetMode       p gpio_mode_output
    pinSetOutputType p gpio_outputtype_pushpull
    pinSetSpeed      p gpio_speed_50mhz
    pinSet           p

----

px4ConsoleTower :: (e -> PX4Platform) -> Tower e ( ChanOutput (Stored Uint8)
                                                 , ChanInput  (Stored Uint8))
px4ConsoleTower topx4 = do
  px4platform <- fmap topx4 getEnv
  uartTower (px4platform_clockconfig . topx4)
            (uart_periph (px4platform_console px4platform))
            (uart_pins   (px4platform_console px4platform))
            115200
            (Proxy :: Proxy 128)

px4platform_clockconfig :: (PX4Platform -> ClockConfig)
px4platform_clockconfig = stm32config_clock . px4platform_stm32config


px4platform_sensorenable_tower :: (e -> PX4Platform)
                               -> ChanOutput (Stored ITime)
                               -> Tower e (ChanOutput (Stored ITime))
px4platform_sensorenable_tower topx4 ready = do
  after_ready <- channel
  px4platform <- fmap topx4 getEnv
  monitor "platform_sensor_enable" $ do
    handler ready "platform_ready" $ do
      e <- emitter (fst after_ready) 1
      callback $ \t -> do
        px4platform_sensorenable px4platform
        emit e t
  return (snd after_ready)
