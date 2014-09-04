module Ivory.BSP.STM32F405.CAN
  ( can1
  , can2
  , canFilters
  ) where

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32.Peripheral.CAN
import Ivory.BSP.STM32F405.Interrupt
import Ivory.BSP.STM32F405.MemoryMap
import Ivory.BSP.STM32F405.RCC

canFilters :: CANPeriphFilters
canFilters = mkCANPeriphFilters can1_periph_base
          (rccEnable rcc_apb1en_can1)
          (rccDisable rcc_apb1en_can1)

can1 :: CANPeriph Interrupt
can1 = mkCANPeriph can1_periph_base
          (rccEnable rcc_apb1en_can1)
          (rccDisable rcc_apb1en_can1)
          CAN1_TX CAN1_RX0 CAN1_RX1 CAN1_SCE
          "can1"

can2 :: CANPeriph Interrupt
can2 = mkCANPeriph can2_periph_base
          (rccEnable rcc_apb1en_can2)
          (rccDisable rcc_apb1en_can2)
          CAN2_TX CAN2_RX0 CAN2_RX1 CAN2_SCE
          "can2"

rccEnable :: BitDataField RCC_APB1ENR Bit -> Ivory eff ()
rccEnable field = modifyReg regRCC_APB1ENR $ setBit field

rccDisable :: BitDataField RCC_APB1ENR Bit -> Ivory eff ()
rccDisable field = modifyReg regRCC_APB1ENR $ clearBit field
