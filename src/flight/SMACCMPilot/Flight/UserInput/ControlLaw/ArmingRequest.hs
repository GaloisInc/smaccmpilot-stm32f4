{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.UserInput.ControlLaw.ArmingRequest
  ( armingInit
  , armingPrimaryRequest
  , armingSecondaryRequest
  ) where

import Ivory.Language
import Ivory.Stdlib

import qualified SMACCMPilot.Flight.Types.ControlLawRequest as R
import qualified SMACCMPilot.Flight.Types.ControlLaw        as L
import qualified SMACCMPilot.Flight.Types.ArmedMode         as A

armingInit :: Ref s (Struct "control_law") -> Ivory eff ()
armingInit law = do
  store (law ~> L.armed_mode)  A.safe

armingPrimaryRequest :: Ref      s1 (Struct "control_law")
                     -> ConstRef s2 (Struct "control_law_request")
                     -> Ivory eff ()
armingPrimaryRequest law req = do
  armed_mode  <- deref (law ~> L.armed_mode)
  safe_req    <- deref (req ~> R.set_safe)
  arm_req     <- deref (req ~> R.set_armed)
  disarm_req  <- deref (req ~> R.set_disarmed)
  cond_
    [ -- Safe mode must be asserted at all times, otherwise we default
      -- to disarmed.
      armed_mode ==? A.safe .&& iNot safe_req ==>
        store (law ~> L.armed_mode) A.disarmed
      -- Never transition from safe to armed on a single event. PPM machine
      -- should enforce a delay between lifting the deadman switch &
      -- completing an arming sequence, but we'll ensure it again here.
    , true ==> cond_
        -- When armed or disarmed, Safe requests dominate, then disarm
        -- requests, then arm requests.
        [ safe_req   ==> store (law ~> L.armed_mode) A.safe
        , disarm_req ==> store (law ~> L.armed_mode) A.disarmed
        , arm_req    ==> store (law ~> L.armed_mode) A.armed
        ]
    ]

armingSecondaryRequest :: Ref      s1 (Struct "control_law")
                       -> ConstRef s2 (Struct "control_law_request")
                       -> Ivory eff ()
armingSecondaryRequest law req = do
  armed_mode  <- deref (law ~> L.armed_mode)
  arm_req     <- deref (req ~> R.set_armed)
  disarm_req  <- deref (req ~> R.set_disarmed)
  -- Secondary is permitted to arm or disarm the vehicle when
  -- the mode is not Safe.
  cond_
    [ armed_mode ==? A.safe ==> return ()
    , disarm_req ==> store (law ~> L.armed_mode) A.disarmed
    , arm_req    ==> store (law ~> L.armed_mode) A.armed
    ]
