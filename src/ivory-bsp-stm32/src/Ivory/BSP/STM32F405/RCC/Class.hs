{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
--
-- Class.hs --- Type class for RCC devices. -- XXX NOT ANYMORE FIX ME LATER
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F405.RCC.Class where

import Ivory.Language
import Ivory.BitData
import Ivory.HW

rccEnable :: (BitData a, IvoryIOReg (BitDataRep a))
          => BitDataReg a -> BitDataField a Bit -> Ivory eff ()
rccEnable reg field = modifyReg reg (setBit field)

rccDisable :: (BitData a, IvoryIOReg (BitDataRep a))
           => BitDataReg a -> BitDataField a Bit -> Ivory eff ()
rccDisable reg field = modifyReg reg (clearBit field)

