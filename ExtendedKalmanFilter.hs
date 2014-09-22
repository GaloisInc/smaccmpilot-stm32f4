module ExtendedKalmanFilter where

import Matrix
import SymDiff

import Data.Foldable (Foldable(..), toList)
import Data.List
import Data.Traversable
import MonadLib (runId, runStateT, sets)

kalmanPredict :: (Eq var, Functor state, Foldable state, Functor dist, Foldable dist) => (state (Sym var) -> dist (Sym var) -> state (Sym var)) -> state var -> dist var -> dist var -> [[Sym var]] -> [[Sym var]]
kalmanPredict process state dist cov p = matBinOp (+) q $ matMult f $ matMult p $ transpose f
    where
    state' = toList $ process (fmap var state) (fmap var dist)
    f = jacobian state' $ toList state
    g = jacobian state' $ toList dist
    q = matMult g $ matMult (diagMat $ map var $ toList cov) $ transpose g

type MeasurementModel state var = ([Sym var], [[Sym var]], state (Sym var), [[Sym var]])
measurementUpdate :: (Traversable state, Eq var) => state var -> [(var, Sym var)] -> [[Sym var]] -> [[Sym var]] -> MeasurementModel state var
measurementUpdate state measurements obsCov errorCov = (innovation, innovCov, state', errorCov')
    where
    innovation = [ var v - h | (v, h) <- measurements ]
    obsModel = jacobian (map snd measurements) (toList state)
    ph = matMult errorCov $ transpose obsModel
    innovCov = matBinOp (+) obsCov $ matMult obsModel ph
    obsGain = matMult ph $ matInvert innovCov
    errorCov' = matBinOp (-) errorCov $ matMult (matMult obsGain obsModel) errorCov

    -- By the above definitions, there must be exactly as many elements in
    -- `state` as in (matVecMult obsGain innovation). Therefore the pattern
    -- matches in this definition can't fail.
    (state', []) = runId $ runStateT (matVecMult obsGain innovation) $ forM state $ \ v -> do
        update <- sets $ \ (x:xs) -> (x, xs)
        return $ var v + update
