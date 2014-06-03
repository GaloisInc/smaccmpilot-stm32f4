{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.BSP.STM32F405.GPIO.Ports
  ( gpioA
  , gpioB
  , gpioC
  , gpioD
  , gpioE
  , gpioF
  , gpioG
  , gpioH
  , gpioI
  ) where

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32.Peripheral.GPIOF4.Peripheral

import Ivory.BSP.STM32F405.RCC
import Ivory.BSP.STM32F405.MemoryMap

-- | Create a GPIO port given the base register address.
mkGPIOPort :: Integer
           -> (forall eff . Ivory eff ())
           -> (forall eff . Ivory eff ())
           -> String
           -> GPIOPort
mkGPIOPort base rccen rccdis n =
  GPIOPort
    { gpioPortMODER          = reg 0x00 "mode"
    , gpioPortOTYPER         = reg 0x04 "otype"
    , gpioPortOSPEEDR        = reg 0x08 "ospeed"
    , gpioPortPUPDR          = reg 0x0C "pupd"
    , gpioPortIDR            = reg 0x10 "idr"
    , gpioPortBSRR           = reg 0x18 "bsrr"
    , gpioPortAFRL           = reg 0x20 "afrl"
    , gpioPortAFRH           = reg 0x24 "afrh"
    , gpioPortRCCEnable      = rccen
    , gpioPortRCCDisable     = rccdis
    , gpioPortName           = n
    }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) (n ++ "->" ++ name)

gpioA :: GPIOPort
gpioA = mkGPIOPort gpioa_periph_base
          (rccEnable rcc_ahb1en_gpioa)
          (rccDisable rcc_ahb1en_gpioa)
          "gpioA"

gpioB :: GPIOPort
gpioB = mkGPIOPort gpiob_periph_base
          (rccEnable rcc_ahb1en_gpiob)
          (rccDisable rcc_ahb1en_gpiob)
          "gpioB"

gpioC :: GPIOPort
gpioC = mkGPIOPort gpioc_periph_base
          (rccEnable rcc_ahb1en_gpioc)
          (rccDisable rcc_ahb1en_gpioc)
          "gpioC"

gpioD :: GPIOPort
gpioD = mkGPIOPort gpiod_periph_base
          (rccEnable rcc_ahb1en_gpiod)
          (rccDisable rcc_ahb1en_gpiod)
          "gpioD"

gpioE :: GPIOPort
gpioE = mkGPIOPort gpioe_periph_base
          (rccEnable rcc_ahb1en_gpioe)
          (rccDisable rcc_ahb1en_gpioe)
          "gpioE"

gpioF :: GPIOPort
gpioF = mkGPIOPort gpiof_periph_base
          (rccEnable rcc_ahb1en_gpiof)
          (rccDisable rcc_ahb1en_gpiof)
          "gpioF"

gpioG :: GPIOPort
gpioG = mkGPIOPort gpiog_periph_base
          (rccEnable rcc_ahb1en_gpiog)
          (rccDisable rcc_ahb1en_gpiog)
          "gpioG"

gpioH :: GPIOPort
gpioH = mkGPIOPort gpioh_periph_base
          (rccEnable rcc_ahb1en_gpioh)
          (rccDisable rcc_ahb1en_gpioh)
          "gpioH"

gpioI :: GPIOPort
gpioI = mkGPIOPort gpioi_periph_base
          (rccEnable rcc_ahb1en_gpioi)
          (rccDisable rcc_ahb1en_gpioi)
          "gpioI"

rccEnable :: BitDataField RCC_AHB1ENR Bit -> Ivory eff ()
rccEnable f = modifyReg regRCC_AHB1ENR $ setBit f
rccDisable :: BitDataField RCC_AHB1ENR Bit -> Ivory eff ()
rccDisable f = modifyReg regRCC_AHB1ENR $ clearBit f


