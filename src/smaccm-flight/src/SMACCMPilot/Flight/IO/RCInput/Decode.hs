{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.IO.RCInput.Decode
  ( RCInputDecoder(..)
  , monitorRCInputDecoder
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib
import Control.Monad (forM_)

import qualified SMACCMPilot.Comm.Ivory.Types.RcInput   as RC
import qualified SMACCMPilot.Comm.Ivory.Types.UserInput as I
import qualified SMACCMPilot.Comm.Ivory.Types.Tristate  as T

import SMACCMPilot.Time
import SMACCMPilot.Flight.IO.RCInput.ModeSwitch
import SMACCMPilot.Flight.IO.RCInput.ArmingMachine

data RCInputDecoder =
  RCInputDecoder
    { rcind_init       :: forall eff    . Ivory eff ()
    , rcind_no_sample  :: forall eff    . ITime -> Ivory eff ()
    , rcind_new_sample :: forall eff s  . ConstRef s ('Struct "rc_input")
                                      -> Ivory eff ()
    , rcind_get_ui     :: forall eff cs . (GetAlloc eff ~ 'Scope cs)
         => Ivory eff (ConstRef ('Stack cs) ('Struct "user_input"))
    , rcind_get_cm_req :: forall eff cs . (GetAlloc eff ~ 'Scope cs)
         => Ivory eff (ConstRef ('Stack cs) ('Struct "control_modes"))
    , rcind_get_am_req :: forall eff cs . (GetAlloc eff ~ 'Scope cs)
         => Ivory eff (ConstRef ('Stack cs) ('Stored T.Tristate))
    }

monitorRCInputDecoder :: Monitor e RCInputDecoder
monitorRCInputDecoder = do
  let named n = fmap showUnique $ freshname $ "rcindecoder_" ++ n
  rcin_last             <- state "rcin_last"

  modeswitch    <- monitorModeSwitch
  armingmachine <- monitorArmingMachine

  init_name <- named "init"
  new_sample_name <- named "new_sample"
  no_sample_name <- named "no_sample"
  get_ui_name <- named "get_ui"
  get_cm_req_name <- named "get_cm_req"
  get_am_req_name <- named "get_am_req"

  let init_proc :: Def('[]':->())
      init_proc = proc init_name $ body $ do
        ms_init modeswitch


      invalidate :: Ivory eff ()
      invalidate = do
          store (rcin_last ~> RC.valid) false
          ms_no_sample modeswitch
          am_no_sample armingmachine

      new_sample_proc :: Def('[ConstRef s ('Struct "rc_input") ]':->())
      new_sample_proc = proc new_sample_name $ \rc_in -> body $ do
        time <- deref (rc_in ~> RC.time)
        all_good <- local (ival true)

        forM_ chan_labels $ \lbl -> do
          ch <- deref (rc_in ~> lbl)
          unless (ch >=? minBound .&& ch <=? maxBound)
                 (store all_good false)

        s <- deref all_good
        unless s $ invalidate
        when   s $ do
          forM_ chan_labels $ \lbl -> do
            (deref (rc_in ~> lbl) >>= store (rcin_last ~> lbl))
          store (rcin_last ~> RC.time) time
          store (rcin_last ~> RC.valid) true
          ms_new_sample modeswitch    rc_in
          am_new_sample armingmachine rc_in

      no_sample_proc :: Def('[ITime]':->())
      no_sample_proc = proc no_sample_name $ \time -> body $ do
        prev <- fmap iTimeFromTimeMicros (deref (rcin_last ~> RC.time))
        when ((time - prev) >? timeout_limit) invalidate

      get_ui_proc :: Def('[Ref s ('Struct "user_input")]':->())
      get_ui_proc = proc get_ui_name $ \ui -> body $ do
        valid <- deref (rcin_last ~> RC.valid)
        time <- fmap iTimeFromTimeMicros (deref (rcin_last ~> RC.time))
        ifte_ valid
          (call_  ppm_decode_ui_proc (constRef rcin_last) ui time)
          (failsafe ui)

      get_cm_req_proc :: Def('[Ref s ('Struct "control_modes")]':->())
      get_cm_req_proc = proc get_cm_req_name $ \cm -> body $ do
        ms_get_req modeswitch cm

      get_am_req_proc :: Def('[Ref s ('Stored T.Tristate)]':->())
      get_am_req_proc = proc get_am_req_name $ \a -> body $ do
        am_get_req armingmachine a

  monitorModuleDef $ do
    incl init_proc
    incl new_sample_proc
    incl no_sample_proc
    incl get_ui_proc
    incl get_cm_req_proc
    incl get_am_req_proc
    incl scale_proc
    incl ppm_decode_ui_proc

  return RCInputDecoder
    { rcind_init       = call_ init_proc
    , rcind_new_sample = call_ new_sample_proc
    , rcind_no_sample  = call_ no_sample_proc
    , rcind_get_ui     = do
        l <- local (istruct [])
        call_ get_ui_proc l
        return (constRef l)
    , rcind_get_cm_req = do
        l <- local izero
        call_ get_cm_req_proc l
        return (constRef l)
    , rcind_get_am_req = do
        l <- local izero
        call_ get_am_req_proc l
        return (constRef l)
    }
  where
  timeout_limit = fromIMilliseconds (150 :: Uint8)-- ms
  chan_labels = [ RC.roll, RC.pitch, RC.throttle, RC.yaw, RC.switch1, RC.switch2 ]

failsafe :: Ref s ('Struct "user_input") -> Ivory eff ()
failsafe ui = do
  store (ui ~> I.roll)      0
  store (ui ~> I.pitch)     0
  store (ui ~> I.throttle) (-1)
  store (ui ~> I.yaw)       0

scale_ppm_channel :: Uint16 -> Ivory eff IFloat
scale_ppm_channel input = call scale_proc center range outmin outmax input
  where
  center = 1500
  range = 500
  outmin = -1.0
  outmax = 1.0



scale_proc :: Def ('[Uint16, Uint16, IFloat, IFloat, Uint16] ':-> IFloat)
scale_proc = proc "ppm_scale_proc" $ \center range outmin outmax input ->
  requires (    (range /=? 0)
            .&& (input >=? minBound)
            .&& (input <=? maxBound)
           )
  $ body $ do
    let centered = safeCast input - safeCast center
    let ranged = centered / safeCast range
    ifte_ (ranged <? outmin)
      (ret outmin)
      (ifte_ (ranged >? outmax)
        (ret outmax)
        (ret ranged))

ppm_decode_ui_proc :: Def ('[ ConstRef s0 ('Struct "rc_input")
                            , Ref s1 ('Struct "user_input")
                            , ITime
                            ] ':-> ())
ppm_decode_ui_proc = proc "ppm_decode_userinput" $ \rcin ui _now ->
  body $ do
  -- Scale 1000-2000 inputs to -1 to 1 inputs.
  let chtransform :: Label "rc_input" ('Stored Uint16)
                  -> Label "user_input" ('Stored IFloat)
                  -> Ivory eff ()
      chtransform rcfield ofield = do
        ppm <- deref (rcin ~> rcfield)
        v   <- scale_ppm_channel ppm
        store (ui ~> ofield) v
  chtransform RC.roll     I.roll
  chtransform RC.pitch    I.pitch
  chtransform RC.throttle I.throttle
  chtransform RC.yaw      I.yaw
  retVoid

