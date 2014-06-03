--
-- AF.hs --- GPIO Alternate Function aliases
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F405.GPIO.AF where

import Ivory.BSP.STM32.Peripheral.GPIOF4.RegTypes

-- These are funky because i did it all with copy paste and regexps based on the
-- gpio.h in hwf4. Yeah, disgusting, I know.

-- Types -----------------------------------------------------------------------

gpio_af_rtc_50hz  :: GPIO_AF
gpio_af_mc0       :: GPIO_AF
gpio_af_tamper    :: GPIO_AF
gpio_af_swj       :: GPIO_AF
gpio_af_trace     :: GPIO_AF

gpio_af_tim1      :: GPIO_AF
gpio_af_tim2      :: GPIO_AF

gpio_af_tim3      :: GPIO_AF
gpio_af_tim4      :: GPIO_AF
gpio_af_tim5      :: GPIO_AF

gpio_af_tim8      :: GPIO_AF
gpio_af_tim9      :: GPIO_AF
gpio_af_tim10     :: GPIO_AF
gpio_af_tim11     :: GPIO_AF

gpio_af_i2c1      :: GPIO_AF
gpio_af_i2c2      :: GPIO_AF
gpio_af_i2c3      :: GPIO_AF

gpio_af_spi1      :: GPIO_AF
gpio_af_spi2      :: GPIO_AF

gpio_af_spi3      :: GPIO_AF

gpio_af_uart1    :: GPIO_AF
gpio_af_uart2    :: GPIO_AF
gpio_af_uart3    :: GPIO_AF
gpio_af_i2s3ext   :: GPIO_AF

gpio_af_uart4     :: GPIO_AF
gpio_af_uart5     :: GPIO_AF
gpio_af_uart6    :: GPIO_AF

gpio_af_can1      :: GPIO_AF
gpio_af_can2      :: GPIO_AF
gpio_af_tim12     :: GPIO_AF
gpio_af_tim13     :: GPIO_AF
gpio_af_tim14     :: GPIO_AF

gpio_af_otg_fs    :: GPIO_AF
gpio_af_otg_hs    :: GPIO_AF

gpio_af_eth       :: GPIO_AF

gpio_af_fsmc       :: GPIO_AF
gpio_af_otg_hs_fs  :: GPIO_AF
gpio_af_sdio       :: GPIO_AF

gpio_af_dcmi       :: GPIO_AF

gpio_af_eventout   :: GPIO_AF

-- Definitions -----------------------------------------------------------------

gpio_af_rtc_50hz = gpio_af0
gpio_af_mc0      = gpio_af0
gpio_af_tamper   = gpio_af0
gpio_af_swj      = gpio_af0
gpio_af_trace    = gpio_af0

gpio_af_tim1     = gpio_af1
gpio_af_tim2     = gpio_af1

gpio_af_tim3     = gpio_af2
gpio_af_tim4     = gpio_af2
gpio_af_tim5     = gpio_af2

gpio_af_tim8     = gpio_af3
gpio_af_tim9     = gpio_af3
gpio_af_tim10    = gpio_af3
gpio_af_tim11    = gpio_af3

gpio_af_i2c1     = gpio_af4
gpio_af_i2c2     = gpio_af4
gpio_af_i2c3     = gpio_af4

gpio_af_spi1     = gpio_af5
gpio_af_spi2     = gpio_af5

gpio_af_spi3     = gpio_af6

gpio_af_uart1    = gpio_af7
gpio_af_uart2    = gpio_af7
gpio_af_uart3    = gpio_af7
gpio_af_i2s3ext  = gpio_af7

gpio_af_uart4    = gpio_af8
gpio_af_uart5    = gpio_af8
gpio_af_uart6    = gpio_af8

gpio_af_can1     = gpio_af9
gpio_af_can2     = gpio_af9
gpio_af_tim12    = gpio_af9
gpio_af_tim13    = gpio_af9
gpio_af_tim14    = gpio_af9

gpio_af_otg_fs   = gpio_af10
gpio_af_otg_hs   = gpio_af10

gpio_af_eth      = gpio_af11

gpio_af_fsmc      = gpio_af12
gpio_af_otg_hs_fs = gpio_af12
gpio_af_sdio      = gpio_af12

gpio_af_dcmi      = gpio_af13

gpio_af_eventout  = gpio_af15

