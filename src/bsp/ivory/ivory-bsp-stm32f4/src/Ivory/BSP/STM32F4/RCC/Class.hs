{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
--
-- Class.hs --- Type class for RCC devices.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.RCC.Class where

import GHC.TypeLits

import Ivory.Language
import Ivory.BitData
import Ivory.HW

import Ivory.BSP.STM32F4.RCC.Regs

class (BitData (RCCEnableReg a),
       IvoryIOReg (BitDataRep (RCCEnableReg a)))
    => RCCDevice a where
  type RCCEnableReg a
  rccDeviceEnableReg   :: a -> BitDataReg (RCCEnableReg a)
  rccDeviceEnableField :: a -> BitDataField (RCCEnableReg a) Bit

rccEnable :: RCCDevice a => a -> Ivory eff ()
rccEnable dev = modifyReg (rccDeviceEnableReg dev) (setBit field)
  where field = rccDeviceEnableField dev
