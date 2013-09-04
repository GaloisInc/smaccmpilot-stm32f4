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
      -> Ivory eff (ConstRef (Stack cs) (Struct "motors"))
mixer control = do
  t <- deref (control ~> C.throttle)
  out <- local (istruct [ M.frontleft  .= ival t
                        , M.frontright .= ival t
                        , M.backleft   .= ival t
                        , M.backright  .= ival t
                        ])
  return (constRef out)

