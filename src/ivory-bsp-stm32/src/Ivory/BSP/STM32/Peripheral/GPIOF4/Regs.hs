{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- GPIOF4/Regs.hs --- GPIO registers for the F4 series
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.GPIOF4.Regs where

import Ivory.Language
import Ivory.BSP.STM32.Peripheral.GPIOF4.RegTypes

-- As this modules shows, having arrays of bit fields in bit data
-- definitions would be a useful addition.
[ivory|
 bitdata GPIO_MODER :: Bits 32 = gpio_moder
  { gpio_mode_15  :: GPIO_Mode
  , gpio_mode_14  :: GPIO_Mode
  , gpio_mode_13  :: GPIO_Mode
  , gpio_mode_12  :: GPIO_Mode
  , gpio_mode_11  :: GPIO_Mode
  , gpio_mode_10  :: GPIO_Mode
  , gpio_mode_9   :: GPIO_Mode
  , gpio_mode_8   :: GPIO_Mode
  , gpio_mode_7   :: GPIO_Mode
  , gpio_mode_6   :: GPIO_Mode
  , gpio_mode_5   :: GPIO_Mode
  , gpio_mode_4   :: GPIO_Mode
  , gpio_mode_3   :: GPIO_Mode
  , gpio_mode_2   :: GPIO_Mode
  , gpio_mode_1   :: GPIO_Mode
  , gpio_mode_0   :: GPIO_Mode
  }

 bitdata GPIO_OTYPER :: Bits 32 = gpio_otyper
  { gpio_otype_15  :: GPIO_OutputType
  , gpio_otype_14  :: GPIO_OutputType
  , gpio_otype_13  :: GPIO_OutputType
  , gpio_otype_12  :: GPIO_OutputType
  , gpio_otype_11  :: GPIO_OutputType
  , gpio_otype_10  :: GPIO_OutputType
  , gpio_otype_9   :: GPIO_OutputType
  , gpio_otype_8   :: GPIO_OutputType
  , gpio_otype_7   :: GPIO_OutputType
  , gpio_otype_6   :: GPIO_OutputType
  , gpio_otype_5   :: GPIO_OutputType
  , gpio_otype_4   :: GPIO_OutputType
  , gpio_otype_3   :: GPIO_OutputType
  , gpio_otype_2   :: GPIO_OutputType
  , gpio_otype_1   :: GPIO_OutputType
  , gpio_otype_0   :: GPIO_OutputType
  }

 bitdata GPIO_OSPEEDR :: Bits 32 = gpio_ospeedr
  { gpio_ospeed_15 :: GPIO_Speed
  , gpio_ospeed_14 :: GPIO_Speed
  , gpio_ospeed_13 :: GPIO_Speed
  , gpio_ospeed_12 :: GPIO_Speed
  , gpio_ospeed_11 :: GPIO_Speed
  , gpio_ospeed_10 :: GPIO_Speed
  , gpio_ospeed_9  :: GPIO_Speed
  , gpio_ospeed_8  :: GPIO_Speed
  , gpio_ospeed_7  :: GPIO_Speed
  , gpio_ospeed_6  :: GPIO_Speed
  , gpio_ospeed_5  :: GPIO_Speed
  , gpio_ospeed_4  :: GPIO_Speed
  , gpio_ospeed_3  :: GPIO_Speed
  , gpio_ospeed_2  :: GPIO_Speed
  , gpio_ospeed_1  :: GPIO_Speed
  , gpio_ospeed_0  :: GPIO_Speed
  }

 bitdata GPIO_PUPDR :: Bits 32 = gpio_pupdr
  { gpio_pupd_15 :: GPIO_PUPD
  , gpio_pupd_14 :: GPIO_PUPD
  , gpio_pupd_13 :: GPIO_PUPD
  , gpio_pupd_12 :: GPIO_PUPD
  , gpio_pupd_11 :: GPIO_PUPD
  , gpio_pupd_10 :: GPIO_PUPD
  , gpio_pupd_9  :: GPIO_PUPD
  , gpio_pupd_8  :: GPIO_PUPD
  , gpio_pupd_7  :: GPIO_PUPD
  , gpio_pupd_6  :: GPIO_PUPD
  , gpio_pupd_5  :: GPIO_PUPD
  , gpio_pupd_4  :: GPIO_PUPD
  , gpio_pupd_3  :: GPIO_PUPD
  , gpio_pupd_2  :: GPIO_PUPD
  , gpio_pupd_1  :: GPIO_PUPD
  , gpio_pupd_0  :: GPIO_PUPD
  }

 bitdata GPIO_IDR :: Bits 32 = gpio_idr
  { gpio_idr_15 :: Bit
  , gpio_idr_14 :: Bit
  , gpio_idr_13 :: Bit
  , gpio_idr_12 :: Bit
  , gpio_idr_11 :: Bit
  , gpio_idr_10 :: Bit
  , gpio_idr_9  :: Bit
  , gpio_idr_8  :: Bit
  , gpio_idr_7  :: Bit
  , gpio_idr_6  :: Bit
  , gpio_idr_5  :: Bit
  , gpio_idr_4  :: Bit
  , gpio_idr_3  :: Bit
  , gpio_idr_2  :: Bit
  , gpio_idr_1  :: Bit
  , gpio_idr_0  :: Bit
  }

 bitdata GPIO_BSRR :: Bits 32 = gpio_bsrr
  -- pin reset bits
  { gpio_br_15 :: Bit
  , gpio_br_14 :: Bit
  , gpio_br_13 :: Bit
  , gpio_br_12 :: Bit
  , gpio_br_11 :: Bit
  , gpio_br_10 :: Bit
  , gpio_br_9  :: Bit
  , gpio_br_8  :: Bit
  , gpio_br_7  :: Bit
  , gpio_br_6  :: Bit
  , gpio_br_5  :: Bit
  , gpio_br_4  :: Bit
  , gpio_br_3  :: Bit
  , gpio_br_2  :: Bit
  , gpio_br_1  :: Bit
  , gpio_br_0  :: Bit
  -- pin set bits
  , gpio_bs_15 :: Bit
  , gpio_bs_14 :: Bit
  , gpio_bs_13 :: Bit
  , gpio_bs_12 :: Bit
  , gpio_bs_11 :: Bit
  , gpio_bs_10 :: Bit
  , gpio_bs_9  :: Bit
  , gpio_bs_8  :: Bit
  , gpio_bs_7  :: Bit
  , gpio_bs_6  :: Bit
  , gpio_bs_5  :: Bit
  , gpio_bs_4  :: Bit
  , gpio_bs_3  :: Bit
  , gpio_bs_2  :: Bit
  , gpio_bs_1  :: Bit
  , gpio_bs_0  :: Bit
  }

 bitdata GPIO_AFRL :: Bits 32 = gpio_afrl
  { gpio_afrl_7  :: GPIO_AF
  , gpio_afrl_6  :: GPIO_AF
  , gpio_afrl_5  :: GPIO_AF
  , gpio_afrl_4  :: GPIO_AF
  , gpio_afrl_3  :: GPIO_AF
  , gpio_afrl_2  :: GPIO_AF
  , gpio_afrl_1  :: GPIO_AF
  , gpio_afrl_0  :: GPIO_AF
  }

 bitdata GPIO_AFRH :: Bits 32 = gpio_afrh
  { gpio_afrh_15 :: GPIO_AF
  , gpio_afrh_14 :: GPIO_AF
  , gpio_afrh_13 :: GPIO_AF
  , gpio_afrh_12 :: GPIO_AF
  , gpio_afrh_11 :: GPIO_AF
  , gpio_afrh_10 :: GPIO_AF
  , gpio_afrh_9  :: GPIO_AF
  , gpio_afrh_8  :: GPIO_AF
  }

|]
