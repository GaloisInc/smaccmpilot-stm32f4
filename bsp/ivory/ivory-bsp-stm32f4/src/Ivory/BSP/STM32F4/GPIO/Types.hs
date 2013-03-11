{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
--
-- GPIO.hs --- GPIO pin driver.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.GPIO.Types where

import Ivory.Language
import Ivory.BitData
import Ivory.HW

import Ivory.BSP.STM32F4.GPIO.RegTypes
import Ivory.BSP.STM32F4.GPIO.Regs
import Ivory.BSP.STM32F4.RCC

-- | A GPIO port, defined as the set of registers that operate on all
-- the pins for that port.
data GPIOPort = GPIOPort
  { gpioPortMODER          :: BitDataReg GPIO_MODER
  , gpioPortOTYPER         :: BitDataReg GPIO_OTYPER
  , gpioPortOSPEEDR        :: BitDataReg GPIO_OSPEEDR
  , gpioPortPUPDR          :: BitDataReg GPIO_PUPDR
  , gpioPortIDR            :: BitDataReg GPIO_IDR
  , gpioPortBSRR           :: BitDataReg GPIO_BSRR
  , gpioPortAFRL           :: BitDataReg GPIO_AFRL
  , gpioPortAFRH           :: BitDataReg GPIO_AFRH
  , gpioPortRCCEnableField :: BitField (RCCEnableReg GPIOPort) Bit
  }

-- | Create a GPIO port given the base register address.
mkGPIOPort :: Integer -> BitField (RCCEnableReg GPIOPort) Bit -> GPIOPort
mkGPIOPort base f =
  GPIOPort
    { gpioPortMODER          = mkBitDataReg $ base + 0x00
    , gpioPortOTYPER         = mkBitDataReg $ base + 0x04
    , gpioPortOSPEEDR        = mkBitDataReg $ base + 0x08
    , gpioPortPUPDR          = mkBitDataReg $ base + 0x0C
    , gpioPortIDR            = mkBitDataReg $ base + 0x10
    , gpioPortBSRR           = mkBitDataReg $ base + 0x18
    , gpioPortAFRL           = mkBitDataReg $ base + 0x20
    , gpioPortAFRH           = mkBitDataReg $ base + 0x24
    , gpioPortRCCEnableField = f
    }

gpioA :: GPIOPort
gpioA = mkGPIOPort 0x40020000 rcc_ahb1en_gpioa

gpioB :: GPIOPort
gpioB = mkGPIOPort 0x40020400 rcc_ahb1en_gpiob

gpioC :: GPIOPort
gpioC = mkGPIOPort 0x40020800 rcc_ahb1en_gpioc

gpioD :: GPIOPort
gpioD = mkGPIOPort 0x40020C00 rcc_ahb1en_gpiod

gpioE :: GPIOPort
gpioE = mkGPIOPort 0x40021000 rcc_ahb1en_gpioe

gpioF :: GPIOPort
gpioF = mkGPIOPort 0x40021400 rcc_ahb1en_gpiof

gpioG :: GPIOPort
gpioG = mkGPIOPort 0x40021800 rcc_ahb1en_gpiog

gpioH :: GPIOPort
gpioH = mkGPIOPort 0x40021C00 rcc_ahb1en_gpioh

gpioI :: GPIOPort
gpioI = mkGPIOPort 0x40022000 rcc_ahb1en_gpioi

instance RCCDevice GPIOPort where
  type RCCEnableReg GPIOPort = RCC_AHB1ENR
  rccDeviceEnableReg _ = regRCC_AHB1ENR
  rccDeviceEnableField = gpioPortRCCEnableField

-- | A GPIO alternate function register and bit field.
data GPIOPinAFR = AFRL (BitField GPIO_AFRL GPIO_AF)
                | AFRH (BitField GPIO_AFRH GPIO_AF)

-- | A GPIO pin, defined as the accessor functions to manipulate the
-- bits in the registers for the port the pin belongs to.
data GPIOPin = GPIOPin
  { gpioPinPort         :: GPIOPort
  , gpioPinMode_F       :: BitField GPIO_MODER GPIO_Mode
  , gpioPinOutputType_F :: BitField GPIO_OTYPER GPIO_OutputType
  , gpioPinSpeed_F      :: BitField GPIO_OSPEEDR GPIO_Speed
  , gpioPinPUPD_F       :: BitField GPIO_PUPDR GPIO_PUPD
  , gpioPinIDR_F        :: BitField GPIO_IDR Bit
  , gpioPinSetBSRR_F    :: BitField GPIO_BSRR Bit
  , gpioPinClearBSRR_F  :: BitField GPIO_BSRR Bit
  , gpioPinAFR_F        :: GPIOPinAFR
  }

-- | Enable the GPIO port for a pin in the RCC.
pinEnable :: GPIOPin -> Ivory s r ()
pinEnable = rccEnable . gpioPinPort

setRegF :: (BitData a, BitValue b, IvoryIOReg (BitFieldRep a))
        => (GPIOPort -> BitDataReg a)
        -> (GPIOPin  -> BitField a b)
        -> GPIOPin
        -> b
        -> Ivory s r ()
setRegF reg field pin val = do
  modifyReg (reg $ gpioPinPort pin) $ do
    setField (field pin) val

pinSetMode :: GPIOPin -> GPIO_Mode -> Ivory s r ()
pinSetMode = setRegF gpioPortMODER gpioPinMode_F

pinSetOutputType :: GPIOPin -> GPIO_OutputType -> Ivory s r ()
pinSetOutputType = setRegF gpioPortOTYPER gpioPinOutputType_F

pinSetSpeed :: GPIOPin -> GPIO_Speed -> Ivory s r ()
pinSetSpeed = setRegF gpioPortOSPEEDR gpioPinSpeed_F

pinSetPUPD :: GPIOPin -> GPIO_PUPD -> Ivory s r ()
pinSetPUPD = setRegF gpioPortPUPDR gpioPinPUPD_F

pinSetAF :: GPIOPin -> GPIO_AF -> Ivory s r ()
pinSetAF pin af =
  case gpioPinAFR_F pin of
    AFRL field -> setRegF gpioPortAFRL (const field) pin af
    AFRH field -> setRegF gpioPortAFRH (const field) pin af

pinSet :: GPIOPin -> Ivory s r ()
pinSet pin =
  modifyReg (gpioPortBSRR $ gpioPinPort pin) $ do
    setBit (gpioPinSetBSRR_F pin)

pinClear :: GPIOPin -> Ivory s r ()
pinClear pin =
  modifyReg (gpioPortBSRR $ gpioPinPort pin) $ do
    setBit (gpioPinClearBSRR_F pin)

-- TODO: pinToggle
-- TODO: pinRead