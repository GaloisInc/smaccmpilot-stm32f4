{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.UserInput.PPM
  ( PPMDecoder(..)
  , taskPPMDecoder
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Flight.Types.UserInput         as I
import qualified SMACCMPilot.Flight.Types.ControlLawRequest as CL

import SMACCMPilot.Flight.UserInput.PPM.ModeSwitch
import SMACCMPilot.Flight.UserInput.PPM.ArmingMachine

data PPMDecoder =
  PPMDecoder
    { ppmd_init       :: forall eff . Ivory eff ()
    , ppmd_no_sample  :: forall eff . Uint32 -> Ivory eff ()
    , ppmd_new_sample :: forall eff s . Ref s I.PPMs -> Uint32 -> Ivory eff ()
    , ppmd_get_ui     :: forall eff cs . (GetAlloc eff ~ Scope cs)
         => Ivory eff (ConstRef (Stack cs) (Struct "userinput_result"))
    , ppmd_get_cl_req :: forall eff cs . (GetAlloc eff ~ Scope cs)
         => Ivory eff (ConstRef (Stack cs) (Struct "control_law_request"))
    }

taskPPMDecoder :: Task p PPMDecoder
taskPPMDecoder = do
  fr <- fresh
  let named n = "ppmdecoder_" ++ n ++ "_" ++ show fr
  ppm_valid             <- taskLocal "ppm_valid"
  ppm_last              <- taskLocal "ppm_last"
  ppm_last_time         <- taskLocal "ppm_last_time"

  modeswitch <- taskModeSwitch
  armingmachine <- taskArmingMachine

  let init_proc :: Def('[]:->())
      init_proc = proc (named "init") $ body $ do
        ms_init modeswitch


      invalidate :: Uint32 -> Ivory eff ()
      invalidate time = do
          store ppm_valid false
          ms_no_sample modeswitch time
          am_no_sample armingmachine time

      new_sample_proc :: Def('[Ref s I.PPMs, Uint32]:->())
      new_sample_proc = proc (named "new_sample") $ \ppms time -> body $ do
        all_good <- local (ival true)
        arrayMap $ \ix -> when (ix <? useful_channels) $ do
          ch <- deref (ppms ! ix)
          unless (ch >=? minBound .&& ch <=? maxBound)
                 (store all_good false)

        s <- deref all_good
        unless s $ invalidate time
        when s $ do
          arrayMap $ \ix -> when (ix <? useful_channels)
            (deref (ppms ! ix) >>= store (ppm_last ! ix))
          store ppm_last_time time
          ms_new_sample modeswitch ppms time
          am_new_sample armingmachine ppms time

      no_sample_proc :: Def('[Uint32]:->())
      no_sample_proc = proc (named "no_sample") $ \time -> body $ do
        prev <- deref ppm_last_time
        when ((time - prev) >? timeout_limit) (invalidate time)

      get_ui_proc :: Def('[Ref s (Struct "userinput_result")]:->())
      get_ui_proc = proc (named "get_ui") $ \ui -> body $ do
        valid <- deref ppm_valid
        time <- deref ppm_last_time
        ifte_ valid
          (call_  ppm_decode_ui_proc ppm_last ui time)
          (failsafe ui)

      get_cl_req_proc :: Def('[Ref s (Struct "control_law_request")]:->())
      get_cl_req_proc = proc (named "get_cl_req") $ \cl_req -> body $ do
        ms_get_cl_req modeswitch cl_req
        am_get_cl_req armingmachine cl_req

  taskModuleDef $ do
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
  timeout_limit = 150 -- ms

failsafe :: Ref s (Struct "userinput_result") -> Ivory eff ()
failsafe ui = do
  store (ui ~> I.roll)      0
  store (ui ~> I.pitch)     0
  store (ui ~> I.throttle) (-1)
  store (ui ~> I.yaw)       0

scale_ppm_channel :: I.PPM -> Ivory eff IFloat
scale_ppm_channel input = call scale_proc I.ppmCenter 500 (-1.0) 1.0 input

scale_proc :: Def ('[I.PPM, Uint16, IFloat, IFloat, I.PPM] :-> IFloat)
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

ppm_decode_ui_proc :: Def ('[ Ref s0 (Array 8 (Stored I.PPM))
                            , Ref s1 (Struct "userinput_result")
                            , Uint32
                            ] :-> ())
ppm_decode_ui_proc = proc "ppm_decode_userinput" $ \ppms ui now ->
  body $ do
  -- Scale 1000-2000 inputs to -1 to 1 inputs.
  let chtransform :: Ix 8
                  -> Label "userinput_result" (Stored IFloat)
                  -> Ivory eff ()
      chtransform ix ofield = do
        ppm <- deref (ppms ! (ix :: Ix 8))
        v   <- scale_ppm_channel ppm
        store (ui ~> ofield) v
  chtransform 0 I.roll
  chtransform 1 I.pitch
  chtransform 2 I.throttle
  chtransform 3 I.yaw
  store (ui ~> I.time) now
  retVoid

