{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Flight.Control.PID where

import Ivory.Language
import Ivory.Stdlib

import qualified SMACCMPilot.Comm.Ivory.Types.PidState as P
import qualified SMACCMPilot.Comm.Ivory.Types.PidConfig as C

controlPIDModule :: Module
controlPIDModule = package "control_pid" $ do
  depend C.pidConfigTypesModule
  depend P.pidStateTypesModule
  incl fconstrain
  incl pid_update
  incl pid_reset

----------------------------------------------------------------------

notFloatNan :: IFloat -> IBool
notFloatNan flt = (iNot $ isnan flt) .&& (iNot $ isinf flt)

-- | Update a PID controller given an error value and measured value
-- and return the output value.
pid_update :: Def ('[ Ref      s1 (Struct "pid_state")
                    , ConstRef s2 (Struct "pid_config")
                    , IFloat
                    , IFloat] :-> IFloat)
pid_update = proc "pid_update" $ \pid cfg err pos ->
  requires (notFloatNan err) $ requires (notFloatNan pos)
  $ body $ do
  p_term  <- fmap (* err) (cfg~>*C.p_gain)
  store (pid~>P.p_last) p_term

  i_min   <- cfg~>*C.i_min
  i_max   <- cfg~>*C.i_max
  i_gain  <- cfg~>*C.i_gain
  pid~>P.i_state %=! (call fconstrain i_min i_max . (+ (err * i_gain)))
  i_term  <- pid~>*P.i_state

  reset      <- pid~>*P.d_reset

  ifte_ reset
    (do store (pid~>P.d_reset) false
        store (pid~>P.d_last)  0)
    (do d_state <- pid~>*P.d_state
        d_gain  <- cfg~>*C.d_gain
        store (pid~>P.d_last) (d_gain * (pos - d_state)))
  store (pid~>P.d_state) pos

  d_term <- deref (pid~>P.d_last)
  ret $ p_term + i_term - d_term

-- | Reset the internal state of a PID.
pid_reset :: Def ('[ Ref s1 (Struct "pid_state") ] :-> ())
pid_reset = proc "pid_reset" $ \pid -> body $ do
  store (pid ~> P.d_reset) true
  store (pid ~> P.i_state) 0.0

-- | Constrain a floating point value to the range [xmin..xmax].
fconstrain :: Def ('[IFloat, IFloat, IFloat] :-> IFloat)
fconstrain = proc "fconstrain" $ \xmin xmax x -> body $
  (ifte_ (x <? xmin)
    (ret xmin)
    (ifte_ (x >? xmax)
      (ret xmax)
      (ret x)))

