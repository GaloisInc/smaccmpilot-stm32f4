{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Platform
  ( flightPlatformParser
  , FlightPlatform(..)
  , fp_clockconfig
  , UART_Device(..)
  , PPM(..)
  , RGBLED_I2C(..)
  , STM32Config
  , ClockConfig
  ) where

import Ivory.Tower.Config

import Data.Char (toUpper)

import qualified Ivory.BSP.STM32F405.UART           as F405
import qualified Ivory.BSP.STM32F405.GPIO           as F405
import qualified Ivory.BSP.STM32F405.GPIO.AF        as F405
import qualified Ivory.BSP.STM32F405.ATIM18         as F405
import qualified Ivory.BSP.STM32F405.Interrupt      as F405
import qualified Ivory.BSP.STM32F427.UART           as F427
import qualified Ivory.BSP.STM32F427.I2C            as F427
import qualified Ivory.BSP.STM32F427.GPIO           as F427
import qualified Ivory.BSP.STM32F427.GPIO.AF        as F427
import           Ivory.BSP.STM32.Peripheral.UART
import           Ivory.BSP.STM32.Peripheral.I2C
import           Ivory.BSP.STM32.Driver.I2C
import           Ivory.BSP.STM32.Interrupt
import           Ivory.BSP.STM32.ClockConfig
import           Ivory.OS.FreeRTOS.Tower.STM32.Config
import           SMACCMPilot.Datalink.Mode
import           SMACCMPilot.Hardware.CAN
import           SMACCMPilot.Hardware.Sensors
import           SMACCMPilot.Hardware.Tests.Platforms (PPM(..), RGBLED_I2C(..))


data FlightPlatform =
  FlightPlatform
    { fp_telem        :: UART_Device
    , fp_ppm          :: PPM
    , fp_sensors      :: Sensors
    , fp_can          :: Maybe CAN_Device
    , fp_datalink     :: DatalinkMode
    , fp_rgbled       :: Maybe RGBLED_I2C
    , fp_stm32config  :: STM32Config
    }

fp_clockconfig :: (FlightPlatform -> ClockConfig)
fp_clockconfig = stm32config_clock . fp_stm32config

flightPlatformParser :: ConfigParser FlightPlatform
flightPlatformParser = do
  p <- subsection "args" $ subsection "platform" string
  case map toUpper p of
    "PX4FMUV17" -> result px4fmuv17
    "PX4FMUV24" -> result px4fmuv24
    "PIXHAWK"   -> result px4fmuv24
    _ -> fail ("no such platform " ++ p)
  where
  result mkPlatform = do
    datalink <- datalinkModeParser DatalinkServer
    let platform = mkPlatform datalink
    conf <- stm32ConfigParser (fp_stm32config platform)
    return platform { fp_stm32config = conf }

px4fmuv17 :: DatalinkMode -> FlightPlatform
px4fmuv17 dmode = FlightPlatform
  { fp_telem       = telem
  , fp_ppm         = ppm
  , fp_sensors     = fmu17_sensors
  , fp_can         = Nothing
  , fp_datalink    = dmode
  , fp_rgbled      = Nothing
  , fp_stm32config = stm32f405Defaults 24
  }
  where
  telem = UART_Device
    { uart_periph = F405.uart5
    , uart_pins = UARTPins
        { uartPinTx = F405.pinC12
        , uartPinRx = F405.pinD2
        , uartPinAF = F405.gpio_af_uart5
        }
    }
  ppm = PPM_Timer F405.tim1 F405.pinA10 F405.gpio_af_tim1 ppm_int
  ppm_int = HasSTM32Interrupt F405.TIM1_CC


px4fmuv24 :: DatalinkMode -> FlightPlatform
px4fmuv24 dmode = FlightPlatform
  { fp_telem       = telem
  , fp_ppm         = PPM_None
  , fp_sensors     = fmu24_sensors
  , fp_can         = Just fmu24_can
  , fp_datalink    = dmode
  , fp_rgbled      = Just rgbled
  , fp_stm32config = stm32f427Defaults 24
  }
  where
  telem = UART_Device
    { uart_periph = F427.uart2
    , uart_pins = UARTPins
        { uartPinTx = F427.pinD5
        , uartPinRx = F427.pinD6
        , uartPinAF = F427.gpio_af_uart2
        }
    }
  -- invariant: rgbled is only device attached to given i2c periph.
  rgbled = RGBLED_I2C
    { rgbled_i2c_periph = F427.i2c2
    , rgbled_i2c_pins = I2CPins
        { i2cpins_sda = F427.pinB11
        , i2cpins_scl = F427.pinB10
        }
    , rgbled_i2c_addr = I2CDeviceAddr 0x55
    }

