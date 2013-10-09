{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module SMACCMPilot.Flight.Control.PID where

import Control.Applicative
import Data.String

import Ivory.Language
import Ivory.Stdlib

controlPIDModule :: Module
controlPIDModule = package "control_pid" $ do
  defStruct (Proxy :: Proxy "PIDState")
  defStruct (Proxy :: Proxy "PIDConfig")
  incl fconstrain
  incl pid_update

----------------------------------------------------------------------
-- Generic PID Controller

[ivory|
  struct PIDState
    { pid_iState :: Stored IFloat
    ; pid_dState :: Stored IFloat
    ; pid_reset  :: Stored Uint8
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

  reset      <- pid~>*pid_reset
  d_term_var <- local (ival 0)

  ifte_ (reset /=? 0)
    (store (pid~>pid_reset) 0)
    (do d_state <- pid~>*pid_dState
        d_gain  <- cfg~>*pid_dGain
        store d_term_var (d_gain * (pos - d_state)))
  store (pid~>pid_dState) pos

  d_term <- deref d_term_var
  ret $ p_term + i_term - d_term

-- | Constrain a floating point value to the range [xmin..xmax].
fconstrain :: Def ('[IFloat, IFloat, IFloat] :-> IFloat)
fconstrain = proc "fconstrain" $ \xmin xmax x -> body $
  (ifte_ (x <? xmin)
    (ret xmin)
    (ifte_ (x >? xmax)
      (ret xmax)
      (ret x)))

