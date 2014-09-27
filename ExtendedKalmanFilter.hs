{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module ExtendedKalmanFilter where

import Matrix

import Data.Foldable (Foldable(..), toList)
import Data.List
import Data.Reflection (Reifies)
import Data.Traversable
import MonadLib (runId, runStateT, sets)
import Numeric.AD.Internal.Reverse (Tape)
import Numeric.AD.Mode.Reverse

kalmanPredict :: (Traversable state, Traversable dist, Num var) => (forall s. Reifies s Tape => state (Reverse s var) -> dist (Reverse s var) -> state (Reverse s var)) -> state var -> dist var -> dist var -> [[var]] -> [[var]]
kalmanPredict process state dist cov = \ p -> matBinOp (+) q $ matMult f $ matMult p $ transpose f
    where
    f = map toList $ jacobian (\ state' -> toList $ process state' (fmap auto dist)) state
    g = map toList $ jacobian (\ dist' -> toList $ process (fmap auto state) dist') dist
    q = matMult g $ matMult (diagMat $ toList cov) $ transpose g

newtype Measurement state var = Measurement { runMeasurement :: forall s. Reifies s Tape => state (Reverse s var) -> Reverse s var }

type MeasurementModel state var = ([var], [[var]], state var, [[var]])
measurementUpdate :: (Traversable state, Fractional var) => state var -> [(var, Measurement state var)] -> [[var]] -> [[var]] -> MeasurementModel state var
measurementUpdate state measurements obsCov errorCov = (innovation, innovCov, state', errorCov')
    where
    (predicted, obsModelVectors) = unzip $ jacobian' (\ state' -> [ runMeasurement h state' | (_, h) <- measurements ]) state
    innovation = zipWith (-) (map fst measurements) predicted
    obsModel = map toList obsModelVectors
    ph = matMult errorCov $ transpose obsModel
    innovCov = matBinOp (+) obsCov $ matMult obsModel ph
    obsGain = matMult ph $ matInvert innovCov
    errorCov' = matBinOp (-) errorCov $ matMult (matMult obsGain obsModel) errorCov

    -- By the above definitions, there must be exactly as many elements in
    -- `state` as in (matVecMult obsGain innovation). Therefore the pattern
    -- matches in this definition can't fail.
    (state', []) = runId $ runStateT (matVecMult obsGain innovation) $ forM state $ \ v -> do
        update <- sets $ \ (x:xs) -> (x, xs)
        return $ v + update
