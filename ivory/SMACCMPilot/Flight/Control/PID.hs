{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module SMACCMPilot.Flight.Control.PID where

import Control.Applicative
import Data.String

import Ivory.Language

import SMACCMPilot.Util.IvoryHelpers
import SMACCMPilot.Param

controlPIDModule :: Module
controlPIDModule = package "control_pid" $ do
  defStruct (Proxy :: Proxy "PID")
  incl fconstrain
  incl pid_update

----------------------------------------------------------------------
-- Generic PID Controller

[ivory|
  struct PID
    { pid_pGain  :: Stored IFloat
    ; pid_iGain  :: Stored IFloat
    ; pid_dGain  :: Stored IFloat
    ; pid_iState :: Stored IFloat
    ; pid_iMin   :: Stored IFloat
    ; pid_iMax   :: Stored IFloat
    ; pid_dState :: Stored IFloat
    ; pid_reset  :: Stored Uint8
  }
|]

notFloatNan :: [IFloat] -> [Cond s r]
notFloatNan = map (\flt -> (check $ (iNot $ isnan flt) .&& (iNot $ isinf flt)))

-- | Update a PID controller given an error value and measured value
-- and return the output value.
pid_update :: Def ('[(Ref s1 (Struct "PID")), IFloat, IFloat] :-> IFloat)
pid_update = proc "pid_update" $ \pid err pos -> body $
  requires (notFloatNan [err, pos]) $
  do
  p_term  <- fmap (* err) (pid~>*pid_pGain)

  i_min   <- pid~>*pid_iMin
  i_max   <- pid~>*pid_iMax
  pid~>pid_iState %=! (call fconstrain i_min i_max . (+ err))
  i_term  <- liftA2 (*) (pid~>*pid_iGain) (pid~>*pid_iState)

  reset      <- pid~>*pid_reset
  d_term_var <- local (ival 0)

  ifte (reset /=? 0)
    (store (pid~>pid_reset) 0)
    (do d_state <- pid~>*pid_dState
        d_gain  <- pid~>*pid_dGain
        store d_term_var (d_gain * (pos - d_state)))
  store (pid~>pid_dState) pos

  d_term <- deref d_term_var
  ret $ p_term + i_term - d_term

-- | Constrain a floating point value to the range [xmin..xmax].
fconstrain :: Def ('[IFloat, IFloat, IFloat] :-> IFloat)
fconstrain = proc "fconstrain" $ \xmin xmax x -> body $
  (ifte (x <? xmin)
    (ret xmin)
    (ifte (x >? xmax)
      (ret xmax)
      (ret x)))

-- | Define a group of parameters for a PID controller.
pid_param_init :: String -> Ref Global (Struct "PID") -> Ivory s r ()
pid_param_init name pid = do
  param_init (fromString $ name ++ "_P")    (pid ~> pid_pGain)
  param_init (fromString $ name ++ "_I")    (pid ~> pid_iGain)
  param_init (fromString $ name ++ "_D")    (pid ~> pid_dGain)
  param_init (fromString $ name ++ "_IMAX") (pid ~> pid_iMax)

instance ParamInit (Struct "PID") where
  param_init = pid_param_init

