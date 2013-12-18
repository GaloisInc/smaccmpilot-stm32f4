{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

--
-- GPIO.hs --- HWF4 GPIO driver interface.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.HWF4.GPIO where

import Ivory.Language

[ivory|

-- | GPIO structure.
abstract struct gpio "hwf4/gpio.h"

-- | Pin structure.
abstract struct pin  "hwf4/gpio.h"

|]

-- | Add external dependencies.
gpioModule :: Module
gpioModule = package "bsp_hwf4wrapper_gpio" $ do
  inclHeader "hwf4/gpio.h"
  defStruct (Proxy :: Proxy "gpio")
  incl gpio_enable
  defMemArea gpio_a
  defMemArea gpio_b
  defMemArea gpio_c
  defMemArea gpio_d
  defMemArea gpio_e
  defMemArea gpio_f
  defMemArea gpio_g
  defMemArea gpio_h
  defMemArea gpio_i

  defStruct (Proxy :: Proxy "pin")
  incl pin_enable
  incl pin_set_mode
  incl pin_set_otype
  incl pin_set_pupd
  incl pin_set_ospeed
  incl pin_set
  incl pin_reset
  incl pin_toggle
  incl pin_read
  defMemArea pin_a0
  defMemArea pin_a1
  defMemArea pin_a2
  defMemArea pin_a3
  defMemArea pin_a4
  defMemArea pin_a5
  defMemArea pin_a6
  defMemArea pin_a7
  defMemArea pin_a8
  defMemArea pin_a9
  defMemArea pin_a10
  defMemArea pin_a11
  defMemArea pin_a12
  defMemArea pin_a13
  defMemArea pin_a14

  defMemArea pin_b0
  defMemArea pin_b1
  defMemArea pin_b2
  defMemArea pin_b3
  defMemArea pin_b4
  defMemArea pin_b5
  defMemArea pin_b6
  defMemArea pin_b7
  defMemArea pin_b8
  defMemArea pin_b9
  defMemArea pin_b10
  defMemArea pin_b11
  defMemArea pin_b12
  defMemArea pin_b13
  defMemArea pin_b14

  defMemArea pin_c0
  defMemArea pin_c1
  defMemArea pin_c2
  defMemArea pin_c3
  defMemArea pin_c4
  defMemArea pin_c5
  defMemArea pin_c6
  defMemArea pin_c7
  defMemArea pin_c8
  defMemArea pin_c9
  defMemArea pin_c10
  defMemArea pin_c11
  defMemArea pin_c12
  defMemArea pin_c13
  defMemArea pin_c14

  defMemArea pin_d0
  defMemArea pin_d1
  defMemArea pin_d2
  defMemArea pin_d3
  defMemArea pin_d4
  defMemArea pin_d5
  defMemArea pin_d6
  defMemArea pin_d7
  defMemArea pin_d8
  defMemArea pin_d9
  defMemArea pin_d10
  defMemArea pin_d11
  defMemArea pin_d12
  defMemArea pin_d13
  defMemArea pin_d14

  defMemArea pin_e0
  defMemArea pin_e1
  defMemArea pin_e2
  defMemArea pin_e3
  defMemArea pin_e4
  defMemArea pin_e5
  defMemArea pin_e6
  defMemArea pin_e7
  defMemArea pin_e8
  defMemArea pin_e9
  defMemArea pin_e10
  defMemArea pin_e11
  defMemArea pin_e12
  defMemArea pin_e13
  defMemArea pin_e14

  defMemArea pin_f0
  defMemArea pin_f1
  defMemArea pin_f2
  defMemArea pin_f3
  defMemArea pin_f4
  defMemArea pin_f5
  defMemArea pin_f6
  defMemArea pin_f7
  defMemArea pin_f8
  defMemArea pin_f9
  defMemArea pin_f10
  defMemArea pin_f11
  defMemArea pin_f12
  defMemArea pin_f13
  defMemArea pin_f14

  defMemArea pin_g0
  defMemArea pin_g1
  defMemArea pin_g2
  defMemArea pin_g3
  defMemArea pin_g4
  defMemArea pin_g5
  defMemArea pin_g6
  defMemArea pin_g7
  defMemArea pin_g8
  defMemArea pin_g9
  defMemArea pin_g10
  defMemArea pin_g11
  defMemArea pin_g12
  defMemArea pin_g13
  defMemArea pin_g14

  defMemArea pin_h0
  defMemArea pin_h1
  defMemArea pin_h2
  defMemArea pin_h3
  defMemArea pin_h4
  defMemArea pin_h5
  defMemArea pin_h6
  defMemArea pin_h7
  defMemArea pin_h8
  defMemArea pin_h9
  defMemArea pin_h10
  defMemArea pin_h11
  defMemArea pin_h12
  defMemArea pin_h13
  defMemArea pin_h14

  defMemArea pin_i0
  defMemArea pin_i1
  defMemArea pin_i2
  defMemArea pin_i3
  defMemArea pin_i4
  defMemArea pin_i5
  defMemArea pin_i6
  defMemArea pin_i7
  defMemArea pin_i8
  defMemArea pin_i9
  defMemArea pin_i10
  defMemArea pin_i11
  defMemArea pin_i12
  defMemArea pin_i13
  defMemArea pin_i14


-- GPIO Bank Interface ---------------------------------------------------------

gpio_enable :: Def ('[ Ref Global (Struct "gpio") ] :-> ())
gpio_enable  = importProc "gpio_enable" "hwf4/gpio.h"


-- GPIO Pin Interface ----------------------------------------------------------

pin_enable :: Def ('[ Ref Global (Struct "pin") ] :-> ())
pin_enable  = importProc "pin_enable" "hwf4/gpio.h"

-- | This is less than elegant, it would be nice to import these values from the
-- header directly.
type PinMode = Uint32
pinModeInput, pinModeOutput, pinModeAF, pinModeAnalog :: PinMode
pinModeInput  = 0x00
pinModeOutput = 0x01
pinModeAF     = 0x02
pinModeAnalog = 0x03

pin_set_mode :: Def ('[ Ref Global (Struct "pin"), PinMode ] :-> ())
pin_set_mode  = importProc "pin_set_mode" "hwf4/gpio.h"


type PinType = Uint32
pinTypePushPull, pinTypePushOpenDrain :: PinType
pinTypePushPull      = 0x0
pinTypePushOpenDrain = 0x1

pin_set_otype :: Def ('[ Ref Global (Struct "pin"), PinType ] :-> ())
pin_set_otype  = importProc "pin_set_otype" "hwf4/gpio.h"


type PinPupdType = Uint32
pinPupdNone, pinPupdUp, pinPupdDown :: PinPupdType
pinPupdNone = 0x00
pinPupdUp   = 0x01
pinPupdDown = 0x02

pin_set_pupd :: Def ('[ Ref Global (Struct "pin"), PinPupdType ] :-> ())
pin_set_pupd  = importProc "pin_set_pupd" "hwf4/gpio.h"

type PinSpeedType = Uint32
pinSpeed2Mhz, pinSpeed25Mhz, pinSpeed50Mhz, pinSpeed100Mhz :: PinSpeedType
pinSpeed2Mhz   = 0x00
pinSpeed25Mhz  = 0x01
pinSpeed50Mhz  = 0x02
pinSpeed100Mhz = 0x03

pin_set_ospeed :: Def ('[ Ref Global (Struct "pin"), PinSpeedType ] :-> ())
pin_set_ospeed  = importProc "pin_set_ospeed" "hwf4/gpio.h"


pin_set :: Def ('[ Ref Global (Struct "pin")] :-> ())
pin_set  = importProc "pin_set" "hwf4/gpio.h"

pin_reset :: Def ('[ Ref Global (Struct "pin")] :-> ())
pin_reset  = importProc "pin_reset" "hwf4/gpio.h"

pin_toggle :: Def ('[ Ref Global (Struct "pin")] :-> ())
pin_toggle = importProc "pin_toggle" "hwf4/gpio.h"

pin_read :: Def ('[ Ref Global (Struct "pin")] :-> IBool)
pin_read = importProc "pin_read" "hwf4/gpio.h"

-- GPIOA -----------------------------------------------------------------------

gpio_a :: MemArea (Struct "gpio")
gpio_a  = importArea "_gpio_a" "hwf4/gpio.h"

pin_a0 :: MemArea (Struct "pin")
pin_a0  = importArea "_pin_a0" "hwf4/gpio.h"

pin_a1 :: MemArea (Struct "pin")
pin_a1  = importArea "_pin_a1" "hwf4/gpio.h"

pin_a2 :: MemArea (Struct "pin")
pin_a2  = importArea "_pin_a2" "hwf4/gpio.h"

pin_a3 :: MemArea (Struct "pin")
pin_a3  = importArea "_pin_a3" "hwf4/gpio.h"

pin_a4 :: MemArea (Struct "pin")
pin_a4  = importArea "_pin_a4" "hwf4/gpio.h"

pin_a5 :: MemArea (Struct "pin")
pin_a5  = importArea "_pin_a5" "hwf4/gpio.h"

pin_a6 :: MemArea (Struct "pin")
pin_a6  = importArea "_pin_a6" "hwf4/gpio.h"

pin_a7 :: MemArea (Struct "pin")
pin_a7  = importArea "_pin_a7" "hwf4/gpio.h"

pin_a8 :: MemArea (Struct "pin")
pin_a8  = importArea "_pin_a8" "hwf4/gpio.h"

pin_a9 :: MemArea (Struct "pin")
pin_a9  = importArea "_pin_a9" "hwf4/gpio.h"

pin_a10 :: MemArea (Struct "pin")
pin_a10  = importArea "_pin_a10" "hwf4/gpio.h"

pin_a11 :: MemArea (Struct "pin")
pin_a11  = importArea "_pin_a11" "hwf4/gpio.h"

pin_a12 :: MemArea (Struct "pin")
pin_a12  = importArea "_pin_a12" "hwf4/gpio.h"

pin_a13 :: MemArea (Struct "pin")
pin_a13  = importArea "_pin_a13" "hwf4/gpio.h"

pin_a14 :: MemArea (Struct "pin")
pin_a14  = importArea "_pin_a14" "hwf4/gpio.h"

pin_a15 :: MemArea (Struct "pin")
pin_a15  = importArea "_pin_a15" "hwf4/gpio.h"


-- GPIOB -----------------------------------------------------------------------

gpio_b :: MemArea (Struct "gpio")
gpio_b  = importArea "_gpio_b" "hwf4/gpio.h"

pin_b0 :: MemArea (Struct "pin")
pin_b0  = importArea "_pin_b0" "hwf4/gpio.h"

pin_b1 :: MemArea (Struct "pin")
pin_b1  = importArea "_pin_b1" "hwf4/gpio.h"

pin_b2 :: MemArea (Struct "pin")
pin_b2  = importArea "_pin_b2" "hwf4/gpio.h"

pin_b3 :: MemArea (Struct "pin")
pin_b3  = importArea "_pin_b3" "hwf4/gpio.h"

pin_b4 :: MemArea (Struct "pin")
pin_b4  = importArea "_pin_b4" "hwf4/gpio.h"

pin_b5 :: MemArea (Struct "pin")
pin_b5  = importArea "_pin_b5" "hwf4/gpio.h"

pin_b6 :: MemArea (Struct "pin")
pin_b6  = importArea "_pin_b6" "hwf4/gpio.h"

pin_b7 :: MemArea (Struct "pin")
pin_b7  = importArea "_pin_b7" "hwf4/gpio.h"

pin_b8 :: MemArea (Struct "pin")
pin_b8  = importArea "_pin_b8" "hwf4/gpio.h"

pin_b9 :: MemArea (Struct "pin")
pin_b9  = importArea "_pin_b9" "hwf4/gpio.h"

pin_b10 :: MemArea (Struct "pin")
pin_b10  = importArea "_pin_b10" "hwf4/gpio.h"

pin_b11 :: MemArea (Struct "pin")
pin_b11  = importArea "_pin_b11" "hwf4/gpio.h"

pin_b12 :: MemArea (Struct "pin")
pin_b12  = importArea "_pin_b12" "hwf4/gpio.h"

pin_b13 :: MemArea (Struct "pin")
pin_b13  = importArea "_pin_b13" "hwf4/gpio.h"

pin_b14 :: MemArea (Struct "pin")
pin_b14  = importArea "_pin_b14" "hwf4/gpio.h"

pin_b15 :: MemArea (Struct "pin")
pin_b15  = importArea "_pin_b15" "hwf4/gpio.h"


-- GPIOC -----------------------------------------------------------------------

gpio_c :: MemArea (Struct "gpio")
gpio_c  = importArea "_gpio_c" "hwf4/gpio.h"

pin_c0 :: MemArea (Struct "pin")
pin_c0  = importArea "_pin_c0" "hwf4/gpio.h"

pin_c1 :: MemArea (Struct "pin")
pin_c1  = importArea "_pin_c1" "hwf4/gpio.h"

pin_c2 :: MemArea (Struct "pin")
pin_c2  = importArea "_pin_c2" "hwf4/gpio.h"

pin_c3 :: MemArea (Struct "pin")
pin_c3  = importArea "_pin_c3" "hwf4/gpio.h"

pin_c4 :: MemArea (Struct "pin")
pin_c4  = importArea "_pin_c4" "hwf4/gpio.h"

pin_c5 :: MemArea (Struct "pin")
pin_c5  = importArea "_pin_c5" "hwf4/gpio.h"

pin_c6 :: MemArea (Struct "pin")
pin_c6  = importArea "_pin_c6" "hwf4/gpio.h"

pin_c7 :: MemArea (Struct "pin")
pin_c7  = importArea "_pin_c7" "hwf4/gpio.h"

pin_c8 :: MemArea (Struct "pin")
pin_c8  = importArea "_pin_c8" "hwf4/gpio.h"

pin_c9 :: MemArea (Struct "pin")
pin_c9  = importArea "_pin_c9" "hwf4/gpio.h"

pin_c10 :: MemArea (Struct "pin")
pin_c10  = importArea "_pin_c10" "hwf4/gpio.h"

pin_c11 :: MemArea (Struct "pin")
pin_c11  = importArea "_pin_c11" "hwf4/gpio.h"

pin_c12 :: MemArea (Struct "pin")
pin_c12  = importArea "_pin_c12" "hwf4/gpio.h"

pin_c13 :: MemArea (Struct "pin")
pin_c13  = importArea "_pin_c13" "hwf4/gpio.h"

pin_c14 :: MemArea (Struct "pin")
pin_c14  = importArea "_pin_c14" "hwf4/gpio.h"

pin_c15 :: MemArea (Struct "pin")
pin_c15  = importArea "_pin_c15" "hwf4/gpio.h"


-- GPIOD -----------------------------------------------------------------------

gpio_d :: MemArea (Struct "gpio")
gpio_d  = importArea "_gpio_d" "hwf4/gpio.h"

pin_d0 :: MemArea (Struct "pin")
pin_d0  = importArea "_pin_d0" "hwf4/gpio.h"

pin_d1 :: MemArea (Struct "pin")
pin_d1  = importArea "_pin_d1" "hwf4/gpio.h"

pin_d2 :: MemArea (Struct "pin")
pin_d2  = importArea "_pin_d2" "hwf4/gpio.h"

pin_d3 :: MemArea (Struct "pin")
pin_d3  = importArea "_pin_d3" "hwf4/gpio.h"

pin_d4 :: MemArea (Struct "pin")
pin_d4  = importArea "_pin_d4" "hwf4/gpio.h"

pin_d5 :: MemArea (Struct "pin")
pin_d5  = importArea "_pin_d5" "hwf4/gpio.h"

pin_d6 :: MemArea (Struct "pin")
pin_d6  = importArea "_pin_d6" "hwf4/gpio.h"

pin_d7 :: MemArea (Struct "pin")
pin_d7  = importArea "_pin_d7" "hwf4/gpio.h"

pin_d8 :: MemArea (Struct "pin")
pin_d8  = importArea "_pin_d8" "hwf4/gpio.h"

pin_d9 :: MemArea (Struct "pin")
pin_d9  = importArea "_pin_d9" "hwf4/gpio.h"

pin_d10 :: MemArea (Struct "pin")
pin_d10  = importArea "_pin_d10" "hwf4/gpio.h"

pin_d11 :: MemArea (Struct "pin")
pin_d11  = importArea "_pin_d11" "hwf4/gpio.h"

pin_d12 :: MemArea (Struct "pin")
pin_d12  = importArea "_pin_d12" "hwf4/gpio.h"

pin_d13 :: MemArea (Struct "pin")
pin_d13  = importArea "_pin_d13" "hwf4/gpio.h"

pin_d14 :: MemArea (Struct "pin")
pin_d14  = importArea "_pin_d14" "hwf4/gpio.h"

pin_d15 :: MemArea (Struct "pin")
pin_d15  = importArea "_pin_d15" "hwf4/gpio.h"


-- GPIOE -----------------------------------------------------------------------

gpio_e :: MemArea (Struct "gpio")
gpio_e  = importArea "_gpio_e" "hwf4/gpio.h"

pin_e0 :: MemArea (Struct "pin")
pin_e0  = importArea "_pin_e0" "hwf4/gpio.h"

pin_e1 :: MemArea (Struct "pin")
pin_e1  = importArea "_pin_e1" "hwf4/gpio.h"

pin_e2 :: MemArea (Struct "pin")
pin_e2  = importArea "_pin_e2" "hwf4/gpio.h"

pin_e3 :: MemArea (Struct "pin")
pin_e3  = importArea "_pin_e3" "hwf4/gpio.h"

pin_e4 :: MemArea (Struct "pin")
pin_e4  = importArea "_pin_e4" "hwf4/gpio.h"

pin_e5 :: MemArea (Struct "pin")
pin_e5  = importArea "_pin_e5" "hwf4/gpio.h"

pin_e6 :: MemArea (Struct "pin")
pin_e6  = importArea "_pin_e6" "hwf4/gpio.h"

pin_e7 :: MemArea (Struct "pin")
pin_e7  = importArea "_pin_e7" "hwf4/gpio.h"

pin_e8 :: MemArea (Struct "pin")
pin_e8  = importArea "_pin_e8" "hwf4/gpio.h"

pin_e9 :: MemArea (Struct "pin")
pin_e9  = importArea "_pin_e9" "hwf4/gpio.h"

pin_e10 :: MemArea (Struct "pin")
pin_e10  = importArea "_pin_e10" "hwf4/gpio.h"

pin_e11 :: MemArea (Struct "pin")
pin_e11  = importArea "_pin_e11" "hwf4/gpio.h"

pin_e12 :: MemArea (Struct "pin")
pin_e12  = importArea "_pin_e12" "hwf4/gpio.h"

pin_e13 :: MemArea (Struct "pin")
pin_e13  = importArea "_pin_e13" "hwf4/gpio.h"

pin_e14 :: MemArea (Struct "pin")
pin_e14  = importArea "_pin_e14" "hwf4/gpio.h"

pin_e15 :: MemArea (Struct "pin")
pin_e15  = importArea "_pin_e15" "hwf4/gpio.h"


-- GPIOF -----------------------------------------------------------------------

gpio_f :: MemArea (Struct "gpio")
gpio_f  = importArea "_gpio_f" "hwf4/gpio.h"

pin_f0 :: MemArea (Struct "pin")
pin_f0  = importArea "_pin_f0" "hwf4/gpio.h"

pin_f1 :: MemArea (Struct "pin")
pin_f1  = importArea "_pin_f1" "hwf4/gpio.h"

pin_f2 :: MemArea (Struct "pin")
pin_f2  = importArea "_pin_f2" "hwf4/gpio.h"

pin_f3 :: MemArea (Struct "pin")
pin_f3  = importArea "_pin_f3" "hwf4/gpio.h"

pin_f4 :: MemArea (Struct "pin")
pin_f4  = importArea "_pin_f4" "hwf4/gpio.h"

pin_f5 :: MemArea (Struct "pin")
pin_f5  = importArea "_pin_f5" "hwf4/gpio.h"

pin_f6 :: MemArea (Struct "pin")
pin_f6  = importArea "_pin_f6" "hwf4/gpio.h"

pin_f7 :: MemArea (Struct "pin")
pin_f7  = importArea "_pin_f7" "hwf4/gpio.h"

pin_f8 :: MemArea (Struct "pin")
pin_f8  = importArea "_pin_f8" "hwf4/gpio.h"

pin_f9 :: MemArea (Struct "pin")
pin_f9  = importArea "_pin_f9" "hwf4/gpio.h"

pin_f10 :: MemArea (Struct "pin")
pin_f10  = importArea "_pin_f10" "hwf4/gpio.h"

pin_f11 :: MemArea (Struct "pin")
pin_f11  = importArea "_pin_f11" "hwf4/gpio.h"

pin_f12 :: MemArea (Struct "pin")
pin_f12  = importArea "_pin_f12" "hwf4/gpio.h"

pin_f13 :: MemArea (Struct "pin")
pin_f13  = importArea "_pin_f13" "hwf4/gpio.h"

pin_f14 :: MemArea (Struct "pin")
pin_f14  = importArea "_pin_f14" "hwf4/gpio.h"

pin_f15 :: MemArea (Struct "pin")
pin_f15  = importArea "_pin_f15" "hwf4/gpio.h"


-- GPIOG -----------------------------------------------------------------------

gpio_g :: MemArea (Struct "gpio")
gpio_g  = importArea "_gpio_g" "hwf4/gpio.h"

pin_g0 :: MemArea (Struct "pin")
pin_g0  = importArea "_pin_g0" "hwf4/gpio.h"

pin_g1 :: MemArea (Struct "pin")
pin_g1  = importArea "_pin_g1" "hwf4/gpio.h"

pin_g2 :: MemArea (Struct "pin")
pin_g2  = importArea "_pin_g2" "hwf4/gpio.h"

pin_g3 :: MemArea (Struct "pin")
pin_g3  = importArea "_pin_g3" "hwf4/gpio.h"

pin_g4 :: MemArea (Struct "pin")
pin_g4  = importArea "_pin_g4" "hwf4/gpio.h"

pin_g5 :: MemArea (Struct "pin")
pin_g5  = importArea "_pin_g5" "hwf4/gpio.h"

pin_g6 :: MemArea (Struct "pin")
pin_g6  = importArea "_pin_g6" "hwf4/gpio.h"

pin_g7 :: MemArea (Struct "pin")
pin_g7  = importArea "_pin_g7" "hwf4/gpio.h"

pin_g8 :: MemArea (Struct "pin")
pin_g8  = importArea "_pin_g8" "hwf4/gpio.h"

pin_g9 :: MemArea (Struct "pin")
pin_g9  = importArea "_pin_g9" "hwf4/gpio.h"

pin_g10 :: MemArea (Struct "pin")
pin_g10  = importArea "_pin_g10" "hwf4/gpio.h"

pin_g11 :: MemArea (Struct "pin")
pin_g11  = importArea "_pin_g11" "hwf4/gpio.h"

pin_g12 :: MemArea (Struct "pin")
pin_g12  = importArea "_pin_g12" "hwf4/gpio.h"

pin_g13 :: MemArea (Struct "pin")
pin_g13  = importArea "_pin_g13" "hwf4/gpio.h"

pin_g14 :: MemArea (Struct "pin")
pin_g14  = importArea "_pin_g14" "hwf4/gpio.h"

pin_g15 :: MemArea (Struct "pin")
pin_g15  = importArea "_pin_g15" "hwf4/gpio.h"


-- GPIOH -----------------------------------------------------------------------

gpio_h :: MemArea (Struct "gpio")
gpio_h  = importArea "_gpio_h" "hwf4/gpio.h"

pin_h0 :: MemArea (Struct "pin")
pin_h0  = importArea "_pin_h0" "hwf4/gpio.h"

pin_h1 :: MemArea (Struct "pin")
pin_h1  = importArea "_pin_h1" "hwf4/gpio.h"

pin_h2 :: MemArea (Struct "pin")
pin_h2  = importArea "_pin_h2" "hwf4/gpio.h"

pin_h3 :: MemArea (Struct "pin")
pin_h3  = importArea "_pin_h3" "hwf4/gpio.h"

pin_h4 :: MemArea (Struct "pin")
pin_h4  = importArea "_pin_h4" "hwf4/gpio.h"

pin_h5 :: MemArea (Struct "pin")
pin_h5  = importArea "_pin_h5" "hwf4/gpio.h"

pin_h6 :: MemArea (Struct "pin")
pin_h6  = importArea "_pin_h6" "hwf4/gpio.h"

pin_h7 :: MemArea (Struct "pin")
pin_h7  = importArea "_pin_h7" "hwf4/gpio.h"

pin_h8 :: MemArea (Struct "pin")
pin_h8  = importArea "_pin_h8" "hwf4/gpio.h"

pin_h9 :: MemArea (Struct "pin")
pin_h9  = importArea "_pin_h9" "hwf4/gpio.h"

pin_h10 :: MemArea (Struct "pin")
pin_h10  = importArea "_pin_h10" "hwf4/gpio.h"

pin_h11 :: MemArea (Struct "pin")
pin_h11  = importArea "_pin_h11" "hwf4/gpio.h"

pin_h12 :: MemArea (Struct "pin")
pin_h12  = importArea "_pin_h12" "hwf4/gpio.h"

pin_h13 :: MemArea (Struct "pin")
pin_h13  = importArea "_pin_h13" "hwf4/gpio.h"

pin_h14 :: MemArea (Struct "pin")
pin_h14  = importArea "_pin_h14" "hwf4/gpio.h"

pin_h15 :: MemArea (Struct "pin")
pin_h15  = importArea "_pin_h15" "hwf4/gpio.h"


-- GPIOI -----------------------------------------------------------------------

gpio_i :: MemArea (Struct "gpio")
gpio_i  = importArea "_gpio_i" "hwf4/gpio.h"

pin_i0 :: MemArea (Struct "pin")
pin_i0  = importArea "_pin_i0" "hwf4/gpio.h"

pin_i1 :: MemArea (Struct "pin")
pin_i1  = importArea "_pin_i1" "hwf4/gpio.h"

pin_i2 :: MemArea (Struct "pin")
pin_i2  = importArea "_pin_i2" "hwf4/gpio.h"

pin_i3 :: MemArea (Struct "pin")
pin_i3  = importArea "_pin_i3" "hwf4/gpio.h"

pin_i4 :: MemArea (Struct "pin")
pin_i4  = importArea "_pin_i4" "hwf4/gpio.h"

pin_i5 :: MemArea (Struct "pin")
pin_i5  = importArea "_pin_i5" "hwf4/gpio.h"

pin_i6 :: MemArea (Struct "pin")
pin_i6  = importArea "_pin_i6" "hwf4/gpio.h"

pin_i7 :: MemArea (Struct "pin")
pin_i7  = importArea "_pin_i7" "hwf4/gpio.h"

pin_i8 :: MemArea (Struct "pin")
pin_i8  = importArea "_pin_i8" "hwf4/gpio.h"

pin_i9 :: MemArea (Struct "pin")
pin_i9  = importArea "_pin_i9" "hwf4/gpio.h"

pin_i10 :: MemArea (Struct "pin")
pin_i10  = importArea "_pin_i10" "hwf4/gpio.h"

pin_i11 :: MemArea (Struct "pin")
pin_i11  = importArea "_pin_i11" "hwf4/gpio.h"

pin_i12 :: MemArea (Struct "pin")
pin_i12  = importArea "_pin_i12" "hwf4/gpio.h"

pin_i13 :: MemArea (Struct "pin")
pin_i13  = importArea "_pin_i13" "hwf4/gpio.h"

pin_i14 :: MemArea (Struct "pin")
pin_i14  = importArea "_pin_i14" "hwf4/gpio.h"

pin_i15 :: MemArea (Struct "pin")
pin_i15  = importArea "_pin_i15" "hwf4/gpio.h"
