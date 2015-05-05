{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.UserInput.PPM.Decode
  ( PPMDecoder(..)
  , monitorPPMDecoder
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Comm.Ivory.Types.UserInput as I
import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw as C

import SMACCMPilot.Flight.UserInput.PPM.ModeSwitch
import SMACCMPilot.Flight.UserInput.PPM.ArmingMachine

data PPMDecoder =
  PPMDecoder
    { ppmd_init       :: forall eff    . Ivory eff ()
    , ppmd_no_sample  :: forall eff    . ITime -> Ivory eff ()
    , ppmd_new_sample :: forall eff s  . Ref s (Array 8 (Stored Uint16))
                                      -> ITime -> Ivory eff ()
    , ppmd_get_ui     :: forall eff cs . (GetAlloc eff ~ Scope cs)
         => Ivory eff (ConstRef (Stack cs) (Struct "user_input"))
    , ppmd_get_cl_req :: forall eff cs . (GetAlloc eff ~ Scope cs)
         => Ivory eff (ConstRef (Stack cs) (Struct "control_law"))
    }

monitorPPMDecoder :: Monitor e PPMDecoder
monitorPPMDecoder = do
  let named n = fmap showUnique $ freshname $ "ppmdecoder_" ++ n
  ppm_valid             <- state "ppm_valid"
  ppm_last              <- state "ppm_last"
  ppm_last_time         <- state "ppm_last_time"

  modeswitch    <- monitorModeSwitch
  armingmachine <- monitorArmingMachine

  init_name <- named "init"
  new_sample_name <- named "new_sample"
  no_sample_name <- named "no_sample"
  get_ui_name <- named "get_ui"
  get_cl_req_name <- named "gel_cl_req"

  let init_proc :: Def('[]:->())
      init_proc = proc init_name $ body $ do
        ms_init modeswitch


      invalidate :: Ivory eff ()
      invalidate = do
          store ppm_valid false
          ms_no_sample modeswitch
          am_no_sample armingmachine

      new_sample_proc :: Def('[Ref s (Array 8 (Stored Uint16)), ITime ]:->())
      new_sample_proc = proc new_sample_name $ \ppms time -> body $ do
        all_good <- local (ival true)
        arrayMap $ \ix -> when (ix <? useful_channels) $ do
          ch <- deref (ppms ! ix)
          unless (ch >=? minBound .&& ch <=? maxBound)
                 (store all_good false)

        s <- deref all_good
        unless s $ invalidate
        when   s $ do
          arrayMap $ \ix -> when (ix <? useful_channels)
            (deref (ppms ! ix) >>= store (ppm_last ! ix))
          store ppm_last_time time
          store ppm_valid true
          ms_new_sample modeswitch ppms time
          am_new_sample armingmachine ppms time

      no_sample_proc :: Def('[ITime]:->())
      no_sample_proc = proc no_sample_name $ \time -> body $ do
        prev <- deref ppm_last_time
        when ((time - prev) >? timeout_limit) invalidate

      get_ui_proc :: Def('[Ref s (Struct "user_input")]:->())
      get_ui_proc = proc get_ui_name $ \ui -> body $ do
        valid <- deref ppm_valid
        time <- deref ppm_last_time
        ifte_ valid
          (call_  ppm_decode_ui_proc ppm_last ui time)
          (failsafe ui)

      get_cl_req_proc :: Def('[Ref s (Struct "control_law")]:->())
      get_cl_req_proc = proc get_cl_req_name $ \cl_req -> body $ do
        ms_get_cl_req modeswitch cl_req
        am_get_cl_req armingmachine (cl_req ~> C.arming_mode)

  monitorModuleDef $ do
    incl init_proc
    incl new_sample_proc
    incl no_sample_proc
    incl get_ui_proc
    incl get_cl_req_proc
    incl scale_proc
    incl ppm_decode_ui_proc

  return PPMDecoder
    { ppmd_init       = call_ init_proc
    , ppmd_new_sample = call_ new_sample_proc
    , ppmd_no_sample  = call_ no_sample_proc
    , ppmd_get_ui     = do
        l <- local (istruct [])
        call_ get_ui_proc l
        return (constRef l)
    , ppmd_get_cl_req = do
        l <- local (istruct [])
        call_ get_cl_req_proc l
        return (constRef l)
    }
  where
  useful_channels = 6
  timeout_limit = fromIMilliseconds (150 :: Uint8)-- ms

failsafe :: Ref s (Struct "user_input") -> Ivory eff ()
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



scale_proc :: Def ('[Uint16, Uint16, IFloat, IFloat, Uint16] :-> IFloat)
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

ppm_decode_ui_proc :: Def ('[ Ref s0 (Array 8 (Stored Uint16))
                            , Ref s1 (Struct "user_input")
                            , ITime
                            ] :-> ())
ppm_decode_ui_proc = proc "ppm_decode_userinput" $ \ppms ui _now ->
  body $ do
  -- Scale 1000-2000 inputs to -1 to 1 inputs.
  let chtransform :: Ix 8
                  -> Label "user_input" (Stored IFloat)
                  -> Ivory eff ()
      chtransform ix ofield = do
        ppm <- deref (ppms ! (ix :: Ix 8))
        v   <- scale_ppm_channel ppm
        store (ui ~> ofield) v
  chtransform 0 I.roll
  chtransform 1 I.pitch
  chtransform 2 I.throttle
  chtransform 3 I.yaw
  retVoid

