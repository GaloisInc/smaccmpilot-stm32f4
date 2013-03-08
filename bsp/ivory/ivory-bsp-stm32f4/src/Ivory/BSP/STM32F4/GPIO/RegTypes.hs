{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
--
-- GPIOTypes.hs --- GPIO pin driver.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.GPIO.RegTypes where

import Ivory.Language
import Ivory.BitData
import Ivory.HW

[bitdata|
 bitvalue GPIO_Mode :: Bits 2
  = GPIO_Mode_Input   as 0
  | GPIO_Mode_Output  as 1
  | GPIO_Mode_AF      as 2
  | GPIO_Mode_Analog  as 3

 bitvalue GPIO_OutputType :: Bits 1
  = GPIO_OutputType_PushPull  as 0
  | GPIO_OutputType_OpenDrain as 1

 bitvalue GPIO_Speed :: Bits 2
  = GPIO_Speed_2MHz   as 0
  | GPIO_Speed_25MHz  as 1
  | GPIO_Speed_50MHz  as 2
  | GPIO_Speed_100MHz as 3

 bitvalue GPIO_PUPD :: Bits 2
  = GPIO_PUPD_None     as 0
  | GPIO_PUPD_PullUp   as 1
  | GPIO_PUPD_PullDown as 2

 bitvalue GPIO_AF :: Bits 4
  = GPIO_AF0  as 0
  | GPIO_AF1  as 1
  | GPIO_AF2  as 2
  | GPIO_AF3  as 3
  | GPIO_AF4  as 4
  | GPIO_AF5  as 5
  | GPIO_AF6  as 6
  | GPIO_AF7  as 7
  | GPIO_AF8  as 8
  | GPIO_AF9  as 9
  | GPIO_AF10 as 10
  | GPIO_AF11 as 11
  | GPIO_AF12 as 12
  | GPIO_AF13 as 13
  | GPIO_AF14 as 14
  | GPIO_AF15 as 15
|]
