{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module SMACCM.INS.ExtendedKalmanFilter where

import Data.Distributive
import Data.Reflection (Reifies)
import Data.Traversable
import Linear
import Numeric.AD.Internal.Reverse (Tape)
import Numeric.AD.Mode.Reverse
import SMACCM.INS.Matrix

kalmanPredict :: (Additive state, Traversable state, Distributive state, Num var)
              => (forall s. Reifies s Tape => state (Reverse s var) -> state (Reverse s var))
              -> state var
              -> state (state var)
              -> state (state var)
              -> (state var, state (state var))
kalmanPredict process state q = \ p -> (state', q !+! f !*! p !*! transpose f)
    where
    predicted = jacobian' process state
    state' = fmap fst predicted
    f = fmap snd predicted

newtype Measurement state var = Measurement { runMeasurement :: forall s. Reifies s Tape => state (Reverse s var) -> Reverse s var }

type MeasurementModel obs state var = (obs var, obs (obs var), state var, state (state var))
measurementUpdate :: (Additive state, Additive obs, Distributive state, Traversable state, Traversable obs, Fractional var)
                  => state var
                  -> obs (var, Measurement state var)
                  -> obs (obs var)
                  -> state (state var)
                  -> MeasurementModel obs state var
measurementUpdate state measurements obsCov errorCov = (innovation, innovCov, state', errorCov')
    where
    predicted = jacobian' (\ stateVars -> fmap (\ (_, h) -> runMeasurement h stateVars) measurements) state
    innovation = fmap fst measurements ^-^ fmap fst predicted
    obsModel = fmap snd predicted
    ph = errorCov !*! transpose obsModel
    innovCov = obsCov !+! obsModel !*! ph
    obsGain = ph !*! matInvert innovCov
    errorCov' = errorCov !-! obsGain !*! obsModel !*! errorCov
    state' = state ^+^ (obsGain !* innovation)
