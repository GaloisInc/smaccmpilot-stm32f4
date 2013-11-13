{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Control.PID where

import Control.Applicative

import Ivory.Language
import Ivory.Stdlib

import SMACCMPilot.Param
import SMACCMPilot.Flight.Param

controlPIDModule :: Module
controlPIDModule = package "control_pid" $ do
  defStruct (Proxy :: Proxy "PIDState")
  defStruct (Proxy :: Proxy "PIDConfig")
  incl fconstrain
  incl pid_update
  incl pid_reset

----------------------------------------------------------------------
-- Generic PID Controller

[ivory|
  struct PIDState
    { pid_iState :: Stored IFloat
    ; pid_dState :: Stored IFloat
    ; pid_dReset :: Stored Uint8
    }

  struct PIDConfig
    { pid_pGain  :: Stored IFloat
    ; pid_iGain  :: Stored IFloat
    ; pid_dGain  :: Stored IFloat
    ; pid_iMin   :: Stored IFloat
    ; pid_iMax   :: Stored IFloat
    }
|]

notFloatNan :: IFloat -> IBool
notFloatNan flt = (iNot $ isnan flt) .&& (iNot $ isinf flt)

-- | Update a PID controller given an error value and measured value
-- and return the output value.
pid_update :: Def ('[ Ref      s1 (Struct "PIDState")
                    , ConstRef s2 (Struct "PIDConfig")
                    , IFloat
                    , IFloat] :-> IFloat)
pid_update = proc "pid_update" $ \pid cfg err pos ->
  requires (notFloatNan err) $ requires (notFloatNan pos)
  $ body $ do
  p_term  <- fmap (* err) (cfg~>*pid_pGain)

  i_min   <- cfg~>*pid_iMin
  i_max   <- cfg~>*pid_iMax
  pid~>pid_iState %=! (call fconstrain i_min i_max . (+ err))
  i_term  <- liftA2 (*) (cfg~>*pid_iGain) (pid~>*pid_iState)

  reset      <- pid~>*pid_dReset
  d_term_var <- local (ival 0)

  ifte_ (reset /=? 0)
    (store (pid~>pid_dReset) 0)
    (do d_state <- pid~>*pid_dState
        d_gain  <- cfg~>*pid_dGain
        store d_term_var (d_gain * (pos - d_state)))
  store (pid~>pid_dState) pos

  d_term <- deref d_term_var
  ret $ p_term + i_term - d_term

-- | Reset the internal state of a PID.
pid_reset :: Def ('[ Ref s1 (Struct "PIDState") ] :-> ())
pid_reset = proc "pid_reset" $ \pid -> body $ do
  store (pid ~> pid_dReset) 1
  store (pid ~> pid_iState) 0.0

-- | Constrain a floating point value to the range [xmin..xmax].
fconstrain :: Def ('[IFloat, IFloat, IFloat] :-> IFloat)
fconstrain = proc "fconstrain" $ \xmin xmax x -> body $
  (ifte_ (x <? xmin)
    (ret xmin)
    (ifte_ (x >? xmax)
      (ret xmax)
      (ret x)))

-- | Read a set of PID parameters into a PIDConfig structure.
getPIDParams :: (GetAlloc eff ~ Scope s2)
             => PIDParams ParamReader
             -> Ref s1 (Struct "PIDConfig")
             -> Ivory eff ()
getPIDParams p ref = do
  storeParam     pid_pGain pidP
  storeParam     pid_iGain pidI
  storeParam     pid_dGain pidD
  storeParam     pid_iMax  pidImax
  storeParamWith pid_iMin  pidImax negate
  where
    storeParamWith slot accessor f = do
      x <- paramRead (accessor p)
      store (ref ~> slot) (f (paramData x))
    storeParam s a = storeParamWith s a id

allocPIDParams :: (GetAlloc eff ~ Scope s)
               => PIDParams ParamReader
               -> Ivory eff (Ref (Stack s) (Struct "PIDConfig"))
allocPIDParams p = do
  cfg <- local izero
  getPIDParams p cfg
  return cfg

