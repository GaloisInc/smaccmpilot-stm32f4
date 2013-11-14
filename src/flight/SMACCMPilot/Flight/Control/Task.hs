{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Control.Task where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib (setJust, cond_, (==>), when)

import qualified SMACCMPilot.Flight.Types.Armed         as A
import qualified SMACCMPilot.Flight.Types.FlightMode    as FM
import qualified SMACCMPilot.Flight.Types.UserInput     as UI
import qualified SMACCMPilot.Flight.Types.Sensors       as SENS
import qualified SMACCMPilot.Flight.Types.ControlOutput as CO

import SMACCMPilot.Param
import SMACCMPilot.Flight.Control.Stabilize
import SMACCMPilot.Flight.Control.AltHold
import SMACCMPilot.Flight.Control.PID     -- XXX
import SMACCMPilot.Flight.Param
import SMACCMPilot.Flight.Types.FlightModeData

controlTask :: (SingI n)
            => DataSink   (Stored A.ArmedMode)
            -> DataSink (Struct "flightmode")
            -> DataSink (Struct "userinput_result")
            -> DataSink (Struct "sensors_result")
            -> ChannelSource n (Struct "controloutput")
            -> DataSource (Struct "alt_hold_state")
            -> FlightParams ParamSink
            -> Task p ()
controlTask a s_fm s_inpt s_sens s_ctl s_ah_state params = do
  armedReader <- withDataReader a "armedReader"
  fmReader    <- withDataReader s_fm   "flightmode"
  uiReader    <- withDataReader s_inpt "userinput"
  sensReader  <- withDataReader s_sens "sensors"
  ahWriter    <- withDataWriter s_ah_state "alt_hold_state"
  ctlEmitter  <- withChannelEmitter s_ctl  "control"

  armRef     <- taskLocal "armed"
  fm         <- taskLocal "flightmode"
  inpt       <- taskLocal "input"
  sens       <- taskLocal "sens"
  ctl        <- taskLocal "control"
  ahState    <- taskLocal "alt_hold"
  prevAlt    <- taskLocal "prev_alt"
  prevMode   <- taskLocal "prev_mode"

  altPID     <- taskLocal "alt_pid"
  ratePID    <- taskLocal "rate_pid"
  accelPID   <- taskLocal "accel_pid"

  -- Generate the stabilization function from the parameter set.
  --
  -- TODO: We can generate more than one function here and switch
  -- between them at the call site to use different PID tunings
  -- depending on the flight mode.
  param_reader      <- paramReader params
  let stabilize_run  = makeStabilizeRun param_reader

  taskInit $ do
    call_ altHoldInit ahState
    writeData ahWriter (constRef ahState)
    store prevAlt 0.0
    store prevMode flightModeStabilize

  let throttleManual :: Def ('[] :-> ())
      throttleManual = proc "throttle_manual" $ body $ do
        thr <- deref (inpt ~> UI.throttle)
        -- -1 =< thr =< 1.  Scale to 0 =< thr' =< 1.
        store (ctl ~> CO.throttle) ((thr + 1) / 2)

  let getAngleBoost :: Def ('[IFloat] :-> IFloat)
      getAngleBoost = proc "get_angle_boost" $ \throttle -> body $ do
        pitch     <- deref (sens ~> SENS.pitch)
        roll      <- deref (sens ~> SENS.roll)
        r22       <- assign (cos pitch * cos roll)
        att_comp  <- local izero

        cond_
          [ r22 >? 0.8
            ==> store att_comp (1.0 / r22)
          , r22 >? 0.0
            ==> store att_comp (((1.0 / 0.8 - 1.0) / 0.8) * r22 + 1.0)
          , true
            ==> store att_comp 1.0
          ]

        thr_out <- fmap (throttle *) (deref att_comp)
        store (ahState ~> ah_angle_boost) (thr_out - throttle)
        ret thr_out

  let throttleAltHold :: Def ('[IFloat] :-> ())
      throttleAltHold = proc "throttle_alt_hold" $ \climb_rate -> body $ do
        let params = flightAltHold param_reader
        getPIDParams (altHoldThrottleAlt   params) altPID
        getPIDParams (altHoldThrottleRate  params) ratePID
        getPIDParams (altHoldThrottleAccel params) accelPID
        stick <- deref (inpt ~> UI.throttle)
        accel <- call altHoldController ahState (constRef inpt)
                      (constRef sens) (constRef altPID) (constRef ratePID)
                      stick climb_rate
        thr   <- call altHoldThrottle ahState (constRef sens)
                      (constRef accelPID) accel
        thr'  <- call getAngleBoost thr
        store (ctl ~> CO.throttle) thr'
        writeData ahWriter (constRef ahState)

  onPeriod 5 $ \now -> do
      alt_current <- deref (sens ~> SENS.baro_alt)
      alt_prev    <- deref prevAlt
      climb_rate  <- assign (alt_current - alt_prev)
      store prevAlt alt_current

      readData sensReader  sens
      readData armedReader armRef
      readData fmReader    fm
      readData uiReader    inpt
      arm <- deref armRef

      call_ stabilize_run arm (constRef fm) (constRef inpt) (constRef sens) ctl

      mode      <- deref (fm ~> FM.mode)
      prev_mode <- deref prevMode

      when (mode /=? prev_mode) $ do
        when (mode ==? flightModeAltHold) $ do
          setJust (ahState ~> ah_target_alt) alt_current
          let params = flightAltHold param_reader
          cruise <- paramRead (altHoldTrimThrottle params)
          store (ahState ~> ah_throttle_cruise) (paramData cruise)

      store prevMode mode

      cond_
        [ mode ==? flightModeAltHold ==> call_ throttleAltHold climb_rate
        , true                       ==> call_ throttleManual
        ]

      emit_ ctlEmitter (constRef ctl)

  taskModuleDef $ do
    depend FM.flightModeTypeModule
    depend UI.userInputTypeModule
    depend SENS.sensorsTypeModule
    depend CO.controlOutputTypeModule
    depend stabilizeControlLoopsModule
    depend altHoldModule
    incl stabilize_run
    incl throttleManual
    incl throttleAltHold
    incl getAngleBoost
