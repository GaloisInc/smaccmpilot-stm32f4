{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Platform
  ( flightPlatformParser
  , FlightPlatform(..)
  , fp_clockconfig
  , UART_Device(..)
  , PPM(..)
  , FlightIO(..)
  , FlightMixer(..)
  , RGBLED_I2C(..)
  , STM32Config
  , ClockConfig
  ) where

import Prelude ()
import Prelude.Compat

import Ivory.Tower.Config

import Data.Char (toUpper)

import qualified Ivory.BSP.STM32F405.UART           as F405
import qualified Ivory.BSP.STM32F405.GPIO           as F405
import qualified Ivory.BSP.STM32F405.GPIO.AF        as F405
import qualified Ivory.BSP.STM32F405.ATIM18         as F405
import qualified Ivory.BSP.STM32F405.Interrupt      as F405
import qualified Ivory.BSP.STM32F427.UART.DMA       as F427
import qualified Ivory.BSP.STM32F427.I2C            as F427
import qualified Ivory.BSP.STM32F427.GPIO           as F427
import qualified Ivory.BSP.STM32F427.GPIO.AF        as F427
import qualified Ivory.BSP.STM32F427.ADC            as F427
import           Ivory.BSP.STM32.Peripheral.UART.DMA
import           Ivory.BSP.STM32.Peripheral.UART
import           Ivory.BSP.STM32.Peripheral.I2C
import           Ivory.BSP.STM32.Driver.I2C
import           Ivory.BSP.STM32.Interrupt
import           Ivory.BSP.STM32.ClockConfig
import           Ivory.BSP.STM32.Config
import           SMACCMPilot.Datalink.Mode
import           SMACCMPilot.Hardware.CAN
import           SMACCMPilot.Hardware.HMC5883L
import           SMACCMPilot.Hardware.Sensors
import           SMACCMPilot.Hardware.Platforms (PPM(..), RGBLED_I2C(..), ADC(..))
import           SMACCMPilot.Hardware.PX4IO (PX4IOPWMConfig(..))
import           SMACCMPilot.Flight.Sensors.LIDARLite
import           SMACCMPilot.Flight.Sensors.PX4Flow
import           SMACCMPilot.Flight.Tuning


data FlightPlatform =
  FlightPlatform
    { fp_telem        :: UART_Device
    , fp_telem_baud   :: Integer
    , fp_gps          :: UART_Device
    , fp_io           :: FlightIO
    , fp_sensors      :: Sensors
    , fp_can          :: Maybe CAN_Device
    , fp_datalink     :: DatalinkMode
    , fp_rgbled       :: Maybe RGBLED_I2C
    , fp_vbatt_adc    :: Maybe ADC
    , fp_tuning       :: FlightTuning
    , fp_mixer        :: FlightMixer
    , fp_stm32config  :: STM32Config
    , fp_lidarlite    :: Maybe LIDARLite
    , fp_px4flow      :: Maybe PX4Flow
    , fp_ext_hmc5883l :: Maybe HMC5883L
    }


data FlightIO
  = PX4IO DMAUART UARTPins PX4IOPWMConfig
  | NativeIO PPM -- No outputs supported with nativeIO right now

data FlightMixer
  = QuadXMixer
  | IrisMixer

fp_clockconfig :: (FlightPlatform -> ClockConfig)
fp_clockconfig = stm32config_clock . fp_stm32config

flightPlatformParser :: ConfigParser FlightPlatform
flightPlatformParser = do
  p <- subsection "args" $ subsection "platform" string
  case map toUpper p of
    "PX4FMUV17" -> result =<< px4fmuv17
    "PX4FMUV24" -> result =<< px4fmuv24
    "PIXHAWK"   -> result =<< px4fmuv24
    _ -> fail ("no such platform " ++ p)
  where
  result platform = do
    conf <- stm32ConfigParser (fp_stm32config platform)
    return platform { fp_stm32config = conf }

mixer :: ConfigParser FlightMixer
mixer = subsection "mixer" $ do
  s <- string
  case map toUpper s of
    "IRIS"  -> return IrisMixer
    "QUADX" -> return QuadXMixer
    _ -> fail ("no such mixer " ++ s ++ ". must be 'iris' or 'quadx'")

telemBaud :: ConfigParser Integer
telemBaud =
  subsection "args" $ subsection "telem_baud" integer <|> pure 57600

px4fmuv17 :: ConfigParser FlightPlatform
px4fmuv17 = do
  vehicle <- subsection "args" $ subsection "vehicle" string
  tuning <- subsection "tuning" $ subsection vehicle flightTuningParser
  m <- subsection "args" mixer
  baud <- telemBaud
  sensors <- fmu17_sensors
  dmode <- datalinkModeParser DatalinkServer
  pure $ FlightPlatform
    { fp_telem        = telem
    , fp_telem_baud   = baud
    , fp_gps          = gps
    , fp_io           = NativeIO ppm
    , fp_sensors      = sensors
    , fp_can          = Nothing
    , fp_datalink     = dmode
    , fp_rgbled       = Nothing
    , fp_vbatt_adc    = Nothing
    , fp_tuning       = tuning
    , fp_mixer        = m
    , fp_stm32config  = stm32f405Defaults 24
    , fp_lidarlite    = Nothing
    , fp_px4flow      = Nothing
    , fp_ext_hmc5883l = Nothing
    }
  where
  telem = UART_Device
    { uart_periph = Left F405.uart5
    , uart_pins = UARTPins
        { uartPinTx = F405.pinC12
        , uartPinRx = F405.pinD2
        , uartPinAF = F405.gpio_af_uart5
        }
    }
  gps = UART_Device
    { uart_periph = Left F405.uart6
    , uart_pins   = UARTPins
      { uartPinTx = F405.pinC6
      , uartPinRx = F405.pinC7
      , uartPinAF = F405.gpio_af_uart6
      }
    }
  ppm = PPM_Timer F405.tim1 F405.pinA10 F405.gpio_af_tim1 ppm_int
  ppm_int = HasSTM32Interrupt F405.TIM1_CC

px4fmuv24 :: ConfigParser FlightPlatform
px4fmuv24 = do
  vehicle <- subsection "args" $ subsection "vehicle" string
  tuning <- subsection "tuning" $ subsection vehicle flightTuningParser
  m <- subsection "args" mixer
  baud <- telemBaud
  sensors <- fmu24_sensors
  mlidar <- fmap Just parseLidar <|> pure Nothing
  mpx4flow <- fmap Just parsePx4flow <|> pure Nothing
  mhmc5883l <- fmap Just parseHMC5883L <|> pure Nothing
  dmode <- datalinkModeParser DatalinkServer
  let pwmconf =
        case map toUpper vehicle of
          "IRIS_PLUS" ->
            PX4IOPWMConfig { px4iopwm_min = 1000, px4iopwm_max = 2000 }
          _ ->
            PX4IOPWMConfig { px4iopwm_min = 1100, px4iopwm_max = 1900 }
      px4io = PX4IO F427.dmaUART6 px4io_pins pwmconf

  return $ FlightPlatform
    { fp_telem        = telem
    , fp_telem_baud   = baud
    , fp_gps          = gps
    , fp_io           = px4io
    , fp_sensors      = sensors
    , fp_can          = Just fmu24_can
    , fp_datalink     = dmode
    , fp_rgbled       = Just rgbled
    , fp_vbatt_adc    = Just adc
    , fp_tuning       = tuning
    , fp_mixer        = m
    , fp_stm32config  = stm32f427Defaults 24
    , fp_lidarlite    = mlidar
    , fp_px4flow      = mpx4flow
    , fp_ext_hmc5883l = mhmc5883l
    }
  where
  telem = UART_Device
    { uart_periph = Right F427.dmaUART2
    , uart_pins = UARTPins
        { uartPinTx = F427.pinD5
        , uartPinRx = F427.pinD6
        , uartPinAF = F427.gpio_af_uart2
        }
    }
  gps = UART_Device
    { uart_periph = Right F427.dmaUART4
    , uart_pins = UARTPins
        { uartPinTx = F427.pinA0
        , uartPinRx = F427.pinA1
        , uartPinAF = F427.gpio_af_uart4
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
  px4io_pins = UARTPins
    { uartPinTx = F427.pinC6
    , uartPinRx = F427.pinC7
    , uartPinAF = F427.gpio_af_uart6
    }

  adc :: ADC
  adc = ADC
    { adc_periph = F427.adc1
    , adc_chan   = 2
    , adc_pin    = F427.pinA2
    , adc_cal    =
        -- This calibration works for Pixhawk #2 on my desk. I assume
        -- they're all close enough to this for now.
        -- TODO: replace with proper calibration, the GAIN 1.27
        -- is only a kludge to get the voltage read correct values
        \x -> (x*0.0100695421)
        -- Linear regression, r squared of .9999:
        -- counts	voltage
        -- 138	0
        -- 1350	11.07
        -- 1420	11.71
        -- 1450	11.92
        -- 1475	12.15
    }
