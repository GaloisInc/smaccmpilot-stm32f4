module SMACCMPilot.Hardware.CAN where

import           Ivory.BSP.STM32.Peripheral.CAN
import           Ivory.BSP.STM32.Peripheral.GPIOF4
import qualified Ivory.BSP.STM32F427.CAN            as F427
import qualified Ivory.BSP.STM32F427.GPIO           as F427

data CAN_Device =
  CAN_Device
    { can_periph  :: CANPeriph
    , can_RX      :: GPIOPin
    , can_TX      :: GPIOPin
    , can_filters :: CANPeriphFilters
    }

fmu24_can :: CAN_Device
fmu24_can = CAN_Device
  { can_periph = F427.can1
  , can_RX = F427.pinD0
  , can_TX = F427.pinD1
  , can_filters = F427.canFilters
  }
