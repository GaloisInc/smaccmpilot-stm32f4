{-# LANGUAGE FlexibleContexts #-}

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

import Ivory.HW
import Ivory.BitData

import Ivory.BSP.STM32.Peripheral.GPIOF4.Peripheral

import Ivory.BSP.STM32F405.RCC
import Ivory.BSP.STM32F405.MemoryMap

-- | Create a GPIO port given the base register address.
mkGPIOPort :: Integer -> BitDataField RCC_AHB1ENR Bit -> String -> GPIOPort
mkGPIOPort base rccfield n =
  GPIOPort
    { gpioPortMODER          = reg 0x00 "mode"
    , gpioPortOTYPER         = reg 0x04 "otype"
    , gpioPortOSPEEDR        = reg 0x08 "ospeed"
    , gpioPortPUPDR          = reg 0x0C "pupd"
    , gpioPortIDR            = reg 0x10 "idr"
    , gpioPortBSRR           = reg 0x18 "bsrr"
    , gpioPortAFRL           = reg 0x20 "afrl"
    , gpioPortAFRH           = reg 0x24 "afrh"
    , gpioPortRCCEnable      = rccEnable  rccreg rccfield
    , gpioPortRCCDisable     = rccDisable rccreg rccfield
    , gpioPortName           = n
    }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) (n ++ "->" ++ name)

  rccreg = regRCC_AHB1ENR -- All GPIO are in AHB1.

gpioA :: GPIOPort
gpioA = mkGPIOPort gpioa_periph_base rcc_ahb1en_gpioa "gpioA"

gpioB :: GPIOPort
gpioB = mkGPIOPort gpiob_periph_base rcc_ahb1en_gpiob "gpioB"

gpioC :: GPIOPort
gpioC = mkGPIOPort gpioc_periph_base rcc_ahb1en_gpioc "gpioC"

gpioD :: GPIOPort
gpioD = mkGPIOPort gpiod_periph_base rcc_ahb1en_gpiod "gpioD"

gpioE :: GPIOPort
gpioE = mkGPIOPort gpioe_periph_base rcc_ahb1en_gpioe "gpioE"

gpioF :: GPIOPort
gpioF = mkGPIOPort gpiof_periph_base rcc_ahb1en_gpiof "gpioF"

gpioG :: GPIOPort
gpioG = mkGPIOPort gpiog_periph_base rcc_ahb1en_gpiog "gpioG"

gpioH :: GPIOPort
gpioH = mkGPIOPort gpioh_periph_base rcc_ahb1en_gpioh "gpioH"

gpioI :: GPIOPort
gpioI = mkGPIOPort gpioi_periph_base rcc_ahb1en_gpioi "gpioI"



