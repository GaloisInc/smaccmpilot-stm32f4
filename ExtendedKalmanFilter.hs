{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module ExtendedKalmanFilter where

import Matrix

import Control.Applicative
import Data.Distributive
import Data.Reflection (Reifies)
import Data.Traversable
import Numeric.AD.Internal.Reverse (Tape)
import Numeric.AD.Mode.Reverse

kalmanPredict :: (Pointwise state, Traversable state, Distributive state, Pointwise dist, Traversable dist, Distributive dist, Num var) => (forall s. Reifies s Tape => state (Reverse s var) -> dist (Reverse s var) -> state (Reverse s var)) -> state var -> dist var -> dist var -> state (state var) -> state (state var)
kalmanPredict process state dist cov = \ p -> matBinOp (+) q $ matMult f $ matMult p $ matTranspose f
    where
    f = jacobian (\ stateVars -> process stateVars (fmap auto dist)) state
    g = jacobian (\ distVars -> process (fmap auto state) distVars) dist
    q = matMult g $ matMult (diagMat cov) $ matTranspose g

newtype Measurement state var = Measurement { runMeasurement :: forall s. Reifies s Tape => state (Reverse s var) -> Reverse s var }

type MeasurementModel obs state var = (obs var, obs (obs var), state var, state (state var))
measurementUpdate :: (Pointwise obs, Traversable obs, Pointwise state, Traversable state, Distributive state, Fractional var) => state var -> obs (var, Measurement state var) -> obs (obs var) -> state (state var) -> MeasurementModel obs state var
measurementUpdate state measurements obsCov errorCov = (innovation, innovCov, state', errorCov')
    where
    predicted = jacobian' (\ stateVars -> fmap (\ (_, h) -> runMeasurement h stateVars) measurements) state
    innovation = (-) <$> fmap fst measurements <*> fmap fst predicted
    obsModel = fmap snd predicted
    ph = matMult errorCov $ matTranspose obsModel
    innovCov = matBinOp (+) obsCov $ matMult obsModel ph
    obsGain = matMult ph $ matInvert innovCov
    errorCov' = matBinOp (-) errorCov $ matMult (matMult obsGain obsModel) errorCov
    state' = (+) <$> state <*> matVecMult obsGain innovation
