{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Hardware.Platforms where

import Prelude ()
import Prelude.Compat

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.Config

import Data.Char (toUpper)

import           SMACCMPilot.Hardware.CAN
import qualified SMACCMPilot.Hardware.PX4FMU17 as FMUv17
import           SMACCMPilot.Hardware.Sensors
import           SMACCMPilot.Hardware.PX4IO (PX4IOPWMConfig(..))

import qualified Ivory.BSP.STM32F405.UART           as F405
import qualified Ivory.BSP.STM32F405.GPIO           as F405
import qualified Ivory.BSP.STM32F405.GPIO.AF        as F405
import qualified Ivory.BSP.STM32F405.ATIM18         as F405
import qualified Ivory.BSP.STM32F405.Interrupt      as F405
import qualified Ivory.BSP.STM32F427.I2C            as F427
import qualified Ivory.BSP.STM32F427.UART.DMA       as F427
import qualified Ivory.BSP.STM32F427.UART           as F427
import qualified Ivory.BSP.STM32F427.GPIO           as F427
import qualified Ivory.BSP.STM32F427.GPIO.AF        as F427
import qualified Ivory.BSP.STM32F427.ADC            as F427
import           Ivory.BSP.STM32.Peripheral.GPIOF4
import           Ivory.BSP.STM32.Peripheral.UART
import           Ivory.BSP.STM32.Peripheral.UART.DMA
import           Ivory.BSP.STM32.Peripheral.SPI
import           Ivory.BSP.STM32.Peripheral.I2C
import           Ivory.BSP.STM32.Peripheral.ATIM18
import           Ivory.BSP.STM32.Peripheral.ADC
import           Ivory.BSP.STM32.Interrupt
import           Ivory.BSP.STM32.Driver.I2C
import           Ivory.BSP.STM32.Driver.UART
import           Ivory.BSP.STM32.Driver.UART.DMA
import           Ivory.BSP.STM32.ClockConfig
import           Ivory.BSP.STM32.Config

data PX4Platform =
  PX4Platform
    { px4platform_gps            :: UART_Device

    , px4platform_sensors        :: Sensors

    , px4platform_motorcontrol   :: forall e . (e -> ClockConfig)
                                 -> ChanOutput ('Array 4 ('Stored IFloat))
                                 -> Tower e ()
    , px4platform_ppm            :: PPM
    , px4platform_px4io          :: PX4IO

    , px4platform_console        :: UART_Device
    , px4platform_can            :: Maybe CAN_Device
    , px4platform_rgbled         :: Maybe RGBLED_I2C
    , px4platform_adc            :: Maybe ADC
    , px4platform_stm32config    :: STM32Config
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
    , ms5611_i2c_pins   :: I2CPins
    , ms5611_i2c_addr   :: I2CDeviceAddr
    }

data MS5611_SPI =
  MS5611_SPI
    { ms5611_spi_device :: SPIDevice
    , ms5611_spi_pins   :: SPIPins
    -- Invariant: Pins are the same as MPU6000, by fiat
    }

data Magnetometer
  = Mag_HMC5883L_I2C HMC5883L_I2C MagCal
  | Mag_LSM303D_SPI  LSM303D_SPI MagCal AccelCal

data HMC5883L_I2C =
  HMC5883L_I2C
    { hmc5883l_i2c_periph :: I2CPeriph
    , hmc5883l_i2c_pins   :: I2CPins
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

data PX4IO
  = PX4IO_Serial DMAUART UARTPins PX4IOPWMConfig
  | PX4IO_None

data RGBLED_I2C =
  RGBLED_I2C
    { rgbled_i2c_periph :: I2CPeriph
    , rgbled_i2c_pins   :: I2CPins
    , rgbled_i2c_addr   :: I2CDeviceAddr
    }


data ADC =
  ADC
    { adc_periph :: ADCPeriph
    , adc_chan   :: Int
    , adc_pin    :: GPIOPin
    , adc_cal    :: IFloat -> IFloat
    }

------

px4platform_mpu6000 :: PX4Platform -> (MPU6000_SPI, AccelCal, GyroCal)
px4platform_mpu6000 PX4Platform{..} = case px4platform_sensors of
  FMU17Sensors{..} ->
      (mpu, fmu17sens_mpu6000_accel_cal, fmu17sens_mpu6000_gyro_cal)
    where
      mpu = MPU6000_SPI
              { mpu6000_spi_device = fmu17sens_mpu6000
              , mpu6000_spi_pins   = fmu17sens_spi_pins
              }
  FMU24Sensors{..} ->
      (mpu, fmu24sens_mpu6000_accel_cal, fmu24sens_mpu6000_gyro_cal)
    where
      mpu = MPU6000_SPI
              { mpu6000_spi_device = fmu24sens_mpu6000
              , mpu6000_spi_pins   = fmu24sens_spi_pins
              }

px4platform_baro :: PX4Platform -> Baro
px4platform_baro PX4Platform{..} = case px4platform_sensors of
  FMU17Sensors{..} -> Baro_MS5611_I2C $ MS5611_I2C
    { ms5611_i2c_periph = fmu17sens_i2c_periph
    , ms5611_i2c_pins   = fmu17sens_i2c_pins
    , ms5611_i2c_addr   = fmu17sens_ms5611
    }
  FMU24Sensors{..} -> Baro_MS5611_SPI $ MS5611_SPI
    { ms5611_spi_device = fmu24sens_ms5611
    , ms5611_spi_pins   = fmu24sens_spi_pins
    }

px4platform_mag :: PX4Platform -> Magnetometer
px4platform_mag PX4Platform{..} = case px4platform_sensors of
  FMU17Sensors{..} ->
    Mag_HMC5883L_I2C
      (HMC5883L_I2C
        { hmc5883l_i2c_periph = fmu17sens_i2c_periph
        , hmc5883l_i2c_pins   = fmu17sens_i2c_pins
        , hmc5883l_i2c_addr   = fmu17sens_hmc5883l
        })
      fmu17sens_hmc5883l_mag_cal
  FMU24Sensors{..} ->
    Mag_LSM303D_SPI
      (LSM303D_SPI
         { lsm303d_spi_device  = fmu24sens_lsm303d
         , lsm303d_spi_pins    = fmu24sens_spi_pins
         })
      fmu24sens_lsm303d_mag_cal
      fmu24sens_lsm303d_accel_cal

px4platform_l3gd20 :: PX4Platform -> Maybe SPIDevice
px4platform_l3gd20 PX4Platform{..} = case px4platform_sensors of
  FMU17Sensors{..} -> Nothing
  FMU24Sensors{..} -> Just fmu24sens_l3gd20

px4platform_sensorenable :: PX4Platform -> Ivory eff ()
px4platform_sensorenable PX4Platform{..} = case px4platform_sensors of
  FMU17Sensors{..} -> return ()
  FMU24Sensors{..} -> fmu24sens_enable
-----------------


px4PlatformParser :: ConfigParser PX4Platform
px4PlatformParser = do
  p <- subsection "args" $ subsection "platform" string
  case map toUpper p of
    "PX4FMUV17"      -> result =<< px4fmuv17
    "PX4FMUV17_IOAR" -> result =<< px4fmuv17_ioar
    "PX4FMUV24"      -> result =<< px4fmuv24
    "PIXHAWK"        -> result =<< px4fmuv24
    _ -> fail ("no such platform " ++ p)
  where
  result platform = do
    conf <- stm32ConfigParser (px4platform_stm32config platform)
    return platform { px4platform_stm32config = conf }

px4fmuv17 :: ConfigParser PX4Platform
px4fmuv17 = do
  sens <- fmu17_sensors
  pure $ PX4Platform
    { px4platform_gps          = gps
    , px4platform_sensors      = sens
    , px4platform_motorcontrol = FMUv17.motorControlTower
    , px4platform_ppm          = ppm_timer
    , px4platform_px4io        = PX4IO_None
    , px4platform_console      = console
    , px4platform_can          = Nothing
    , px4platform_rgbled       = Nothing
    , px4platform_adc          = Nothing
    , px4platform_stm32config  = stm32f405Defaults 24
    }
  where
  console :: UART_Device
  console = UART_Device
    { uart_periph = Left F405.uart5
    , uart_pins = UARTPins
        { uartPinTx = F405.pinC12
        , uartPinRx = F405.pinD2
        , uartPinAF = F405.gpio_af_uart5
        }
    }
  gps :: UART_Device
  gps = UART_Device
    { uart_periph = Left F405.uart6
    , uart_pins   = UARTPins
      { uartPinTx = F405.pinC6
      , uartPinRx = F405.pinC7
      , uartPinAF = F405.gpio_af_uart6
      }
    }
  ppm_timer :: PPM
  ppm_timer = PPM_Timer F405.tim1 F405.pinA10 F405.gpio_af_tim1 ppm_int
  ppm_int = HasSTM32Interrupt F405.TIM1_CC

px4fmuv17_ioar :: ConfigParser PX4Platform
px4fmuv17_ioar = do
  p <- px4fmuv17
  return $ p { px4platform_console = console }
  where
  console :: UART_Device
  console = UART_Device
    { uart_periph = Left F405.uart1
    , uart_pins   = UARTPins
        { uartPinTx = F405.pinB6
        , uartPinRx = F405.pinB7
        , uartPinAF = F405.gpio_af_uart1
        }
    }

px4fmuv24 :: ConfigParser PX4Platform
px4fmuv24 = do
  sens <- fmu24_sensors
  return $ PX4Platform
    { px4platform_gps          = gps
    , px4platform_sensors      = sens
    , px4platform_motorcontrol = error "motor control not defined for px4fmuv24"
    , px4platform_ppm          = PPM_None
    , px4platform_px4io        = px4io
    , px4platform_console      = console
    , px4platform_can          = Just fmu24_can
    , px4platform_rgbled       = Just rgbled
    , px4platform_adc          = Just adc
    , px4platform_stm32config  = stm32f427Defaults 24
    }
  where
  console = UART_Device -- Telem 1 Port
    { uart_periph = Right F427.dmaUART2
    , uart_pins = UARTPins
        { uartPinTx = F427.pinD5
        , uartPinRx = F427.pinD6
        , uartPinAF = F427.gpio_af_uart2
        }
    }
  gps = UART_Device
    { uart_periph = Left F427.uart4
    , uart_pins = UARTPins
        { uartPinTx = F427.pinA0
        , uartPinRx = F427.pinA1
        , uartPinAF = F427.gpio_af_uart4
        }
    }
  rgbled = RGBLED_I2C
    { rgbled_i2c_periph = F427.i2c2
    , rgbled_i2c_pins = I2CPins
        { i2cpins_sda = F427.pinB11
        , i2cpins_scl = F427.pinB10
        }
    , rgbled_i2c_addr = I2CDeviceAddr 0x55
    }
  px4io = PX4IO_Serial F427.dmaUART6 px4io_pins pwmconf
  px4io_pins = UARTPins
    { uartPinTx = F427.pinC6
    , uartPinRx = F427.pinC7
    , uartPinAF = F427.gpio_af_uart6
    }
  pwmconf = PX4IOPWMConfig
    { px4iopwm_min = 1000
    , px4iopwm_max = 2000
    }
  adc = ADC
    { adc_periph = F427.adc1
    , adc_chan   = 2
    , adc_pin    = F427.pinA2
    , adc_cal    =
        -- This calibration works for Pixhawk #2 on my desk. I assume
        -- they're all close enough to this for now.
        \x -> ((x - 137.718) / 109.709)
        -- Linear regression, r squared of .9999:
        -- counts	voltage
        -- 138	0
        -- 1350	11.07
        -- 1420	11.71
        -- 1450	11.92
        -- 1475	12.15
    }

----

[ivory| string struct ConsoleBuffer 256 |]

px4ConsoleTower :: (e -> PX4Platform)
                -> Tower e
                  ( BackpressureTransmit ConsoleBuffer ('Stored IBool)
                  , ChanOutput ('Stored Uint8)
                  , Monitor e ())
px4ConsoleTower topx4 = do
  let consoleModule = package "px4_console" $ defStringType (Proxy :: Proxy ConsoleBuffer)
  towerModule consoleModule
  towerDepends consoleModule

  px4platform <- fmap topx4 getEnv
  case uart_periph (px4platform_console px4platform) of
    Left uart ->
      uartTower (px4platform_clockconfig . topx4)  uart
              (uart_pins   (px4platform_console px4platform))
              115200
    Right dmauart ->
      dmaUARTTower (px4platform_clockconfig . topx4) dmauart
              (uart_pins   (px4platform_console px4platform))
              115200 (Proxy :: Proxy ConsoleBuffer)

px4platform_clockconfig :: (PX4Platform -> ClockConfig)
px4platform_clockconfig = stm32config_clock . px4platform_stm32config


px4platform_sensorenable_tower :: (e -> PX4Platform)
                               -> ChanOutput ('Stored ITime)
                               -> Tower e (ChanOutput ('Stored ITime))
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
