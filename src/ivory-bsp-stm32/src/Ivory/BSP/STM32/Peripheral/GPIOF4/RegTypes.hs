{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- RegTypes.hs --- Types for register fields in GPIO pin driver.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.GPIOF4.RegTypes where

import Ivory.Language

[ivory|
 bitdata GPIO_Mode :: Bits 2
  = gpio_mode_input   as 0
  | gpio_mode_output  as 1
  | gpio_mode_af      as 2
  | gpio_mode_analog  as 3

 bitdata GPIO_OutputType :: Bits 1
  = gpio_outputtype_pushpull  as 0
  | gpio_outputtype_opendrain as 1

 bitdata GPIO_Speed :: Bits 2
  = gpio_speed_2mhz   as 0
  | gpio_speed_25mhz  as 1
  | gpio_speed_50mhz  as 2
  | gpio_speed_100mhz as 3

 bitdata GPIO_PUPD :: Bits 2
  = gpio_pupd_none     as 0
  | gpio_pupd_pullup   as 1
  | gpio_pupd_pulldown as 2

 bitdata GPIO_AF :: Bits 4
  = gpio_af0  as 0
  | gpio_af1  as 1
  | gpio_af2  as 2
  | gpio_af3  as 3
  | gpio_af4  as 4
  | gpio_af5  as 5
  | gpio_af6  as 6
  | gpio_af7  as 7
  | gpio_af8  as 8
  | gpio_af9  as 9
  | gpio_af10 as 10
  | gpio_af11 as 11
  | gpio_af12 as 12
  | gpio_af13 as 13
  | gpio_af14 as 14
  | gpio_af15 as 15
|]
