{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.IO.RCInput
  ( rcInputTower
  ) where

import Ivory.Language
import Ivory.Tower

import qualified SMACCMPilot.Comm.Ivory.Types.UserInput     as UI
import qualified SMACCMPilot.Comm.Ivory.Types.UserInputTrim as T
import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw as C ()
import qualified SMACCMPilot.Comm.Ivory.Types.RcInput    as RC
import qualified SMACCMPilot.Comm.Ivory.Types.Tristate   as T
import           SMACCMPilot.Comm.Tower.Attr
import           SMACCMPilot.Comm.Tower.Interface.ControllableVehicle

import SMACCMPilot.Flight.IO.RCInput.Decode

rcInputTower :: ControllableVehicleAttrs Attr
             -> ChanOutput (Struct "rc_input")
             -> ChanInput (Struct "user_input")
             -> ChanInput (Struct "control_modes")
             -> ChanInput (Stored T.Tristate)
             -> Tower e ()
rcInputTower attrs rc ui cmr amr = do
  p <- period (Milliseconds 50)

  monitor "rcin_userinput_translator" $ do
    rcin       <- state "rcin"
    valid      <- state "valid"
    decoder    <- monitorRCInputDecoder
    trim       <- attrState (userInputTrim attrs)

    handler systemInit "userinput_init" $ callback $ const $ do
      rcind_init decoder
      store valid false

    handler rc "rcin_userinput_capt" $ do
      e <- attrEmitter (rcInput attrs)
      callback $ \rc_in -> do
        refCopy rcin rc_in
        refCopy valid (rc_in ~> RC.valid)
        emit e rc_in

    handler p "periodic_userinput_decode" $ do
      -- Invariant: multiplexer depends on message clr being delivered before ui.
      -- Tower should deliver messages in order of the emitter declarations.
      -- cl and ui are emitted on every period tick.
      cmr_emitter <- emitter cmr 1
      amr_emitter <- emitter amr 1
      ui_emitter <- emitter ui 1
      callbackV $ \now -> do
        v <- deref valid
        ifte_ v
              (rcind_new_sample decoder (constRef rcin))
              (rcind_no_sample decoder now)
        rcind_get_ui     decoder >>= uitrim trim >>= emit ui_emitter
        rcind_get_cm_req decoder >>= emit cmr_emitter
        rcind_get_am_req decoder >>= emit amr_emitter
        store valid false


uitrim :: (GetAlloc eff ~ Scope s3)
       => Ref      s1 (Struct "user_input_trim")
       -> ConstRef s2 (Struct "user_input")
       -> Ivory eff (ConstRef (Stack s3) (Struct "user_input"))
uitrim trim ui = do
  t <- stick_trim T.throttle UI.throttle
  r <- stick_trim T.roll UI.roll
  p <- stick_trim T.pitch UI.pitch
  y <- stick_trim T.yaw UI.yaw
  ui' <- local $ istruct
    [ UI.throttle .= ival t
    , UI.roll     .= ival r
    , UI.pitch    .= ival p
    , UI.yaw      .= ival y
    ]
  return (constRef ui')

  where

  stick_constrain s = ((s >? 1.0) ? (1.0, (s <? (-1.0)) ? (-1.0, s)))

  stick_trim tlbl ulbl = do
    t <- deref (trim ~> tlbl)
    u <- deref (ui   ~> ulbl)
    let trimmed = stick_constrain (u + t)
    return ((t <? 1.0 .&& t >? -1.0) ? (trimmed, u))
