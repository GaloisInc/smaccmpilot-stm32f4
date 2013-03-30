{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
--
-- Class.hs --- Type class for RCC devices.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.RCC.Class where

import Ivory.Language
import Ivory.BitData
import Ivory.HW

import Ivory.BSP.STM32F4.RCC.Regs

class (IvoryBitRep (BitFieldRep (RCCEnableReg a)),
       IvoryIOReg (BitFieldRep (RCCEnableReg a)))
    => RCCDevice a where
  type RCCEnableReg a
  rccDeviceEnableReg :: a -> BitDataReg (RCCEnableReg a)
  rccDeviceEnableField :: a -> BitField (RCCEnableReg a) Bit

rccEnable :: RCCDevice a => a -> Ivory eff ()
rccEnable dev = do
  modifyReg (rccDeviceEnableReg dev) (setBit (rccDeviceEnableField dev))
