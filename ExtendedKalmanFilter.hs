module ExtendedKalmanFilter where

import Matrix
import SymDiff

import Data.Foldable (Foldable(..), toList)
import Data.List

kalmanPredict :: (Eq var, Functor state, Foldable state, Functor dist, Foldable dist) => (state (Sym var) -> dist (Sym var) -> state (Sym var)) -> state var -> dist var -> dist var -> [[Sym var]] -> [[Sym var]]
kalmanPredict process state dist cov p = matBinOp (+) q $ matMult f $ matMult p $ transpose f
    where
    state' = toList $ process (fmap var state) (fmap var dist)
    f = jacobian state' $ toList state
    g = jacobian state' $ toList dist
    q = matMult g $ matMult (diagMat $ map var $ toList cov) $ transpose g

type MeasurementModel var = ([Sym var], [(var, Sym var)], [[Sym var]])
measurementUpdate :: (Foldable t, Eq var) => t var -> [(var, Sym var)] -> [[Sym var]] -> [[Sym var]] -> MeasurementModel var
measurementUpdate state measurements obsCov errorCov = (innovation, state', errorCov')
    where
    innovation = [ var v - h | (v, h) <- measurements ]
    obsModel = jacobian (map snd measurements) (toList state)
    ph = matMult errorCov $ transpose obsModel
    obsGain = matMult ph $ matInvert $ matBinOp (+) obsCov $ matMult obsModel ph
    state' = [ (v, var v + update) | (v, update) <- zip (toList state) (matVecMult obsGain innovation) ]
    errorCov' = matBinOp (-) errorCov $ matMult (matMult obsGain obsModel) errorCov
