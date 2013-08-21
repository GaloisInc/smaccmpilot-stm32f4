{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Motors.Mixing
  ( mixer
  ) where

import Ivory.Language

import qualified SMACCMPilot.Flight.Types.ControlOutput as C
import qualified SMACCMPilot.Flight.Types.Motors        as M

mixer :: (GetAlloc eff ~ Scope cs)
      => ConstRef s1 (Struct "controloutput")
      -> Uint32
      -> Ivory eff (ConstRef (Stack cs) (Struct "motors"))
mixer control t = local (istruct [M.time .= ival t]) >>= (return . constRef)

