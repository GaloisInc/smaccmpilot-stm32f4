
module Ivory.BSP.STM32F405.I2C
  ( i2c1
  , i2c2
  , i2c3
  ) where

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32.Peripheral.I2C
import Ivory.BSP.STM32F405.Interrupt
import Ivory.BSP.STM32F405.MemoryMap
import Ivory.BSP.STM32F405.RCC

i2c1 :: I2CPeriph Interrupt
i2c1 = mkI2CPeriph i2c1_periph_base
          (rccEnable rcc_apb1en_i2c1) (rccDisable rcc_apb1en_i2c1)
          I2C1_EV I2C1_ER "i2c1"

i2c2 :: I2CPeriph Interrupt
i2c2 = mkI2CPeriph i2c2_periph_base
          (rccEnable rcc_apb1en_i2c2)
          (rccDisable rcc_apb1en_i2c2)
          I2C2_EV I2C2_ER "i2c2"

i2c3 :: I2CPeriph Interrupt
i2c3 = mkI2CPeriph i2c3_periph_base
          (rccEnable rcc_apb1en_i2c3) (rccDisable rcc_apb1en_i2c3)
          I2C3_EV I2C3_ER "i2c3"

rccEnable :: BitDataField RCC_APB1ENR Bit -> Ivory eff ()
rccEnable field = modifyReg regRCC_APB1ENR $ setBit field

rccDisable :: BitDataField RCC_APB1ENR Bit -> Ivory eff ()
rccDisable field = modifyReg regRCC_APB1ENR $ clearBit field


