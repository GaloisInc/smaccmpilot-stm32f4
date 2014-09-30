module SMACCM.INS.Simulate where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import MonadLib (runStateT, StateT, get, set)
import Numeric.AD
import Prelude hiding (mapM, sequence, sum)
import SMACCM.INS.ExtendedKalmanFilter
import SMACCM.INS.SensorFusionModel
import SMACCM.INS.Vec3

type KalmanState m a = StateT (a, StateVector a, StateVector (StateVector a)) m

runKalmanState :: (Monad m, Fractional a) => a -> StateVector a -> KalmanState m a b -> m (b, (a, StateVector a, StateVector (StateVector a)))
runKalmanState ts state = runStateT (ts, state, kalmanP)

fixQuat :: Floating a => StateVector a -> StateVector a
fixQuat state = (pure id) { stateOrient = pure (/ quatMag) } <*> state
    where
    quatMag = sqrt $ sum $ fmap (^ (2 :: Int)) $ stateOrient state

runProcessModel :: (Monad m, Floating a) => a -> DisturbanceVector a -> KalmanState m a ()
runProcessModel dt dist = do
    (ts, state, p) <- get
    let state' = processModel dt state dist
    let p' = kalmanPredict (processModel $ auto dt) state dist (distCovariance dt) p
    set (ts, fixQuat state', p')

runFusion :: (Monad m, Floating a) => (a -> StateVector a -> StateVector (StateVector a) -> (a, a, StateVector a, StateVector (StateVector a))) -> a -> KalmanState m a (a, a)
runFusion fuse measurement = do
    (ts, state, p) <- get
    let (innov, innovCov, state', p') = fuse measurement state p
    set (ts, fixQuat state', p')
    return (innov, innovCov)

runFuseVel :: (Monad m, Floating a, Real a) => NED a -> KalmanState m a (NED (a, a))
runFuseVel measurement = sequence $ runFusion <$> (fuseVel <*> velNoise) <*> measurement

runFusePos :: (Monad m, Floating a, Real a) => NED a -> KalmanState m a (NED (a, a))
runFusePos measurement = sequence $ runFusion <$> (fusePos <*> posNoise) <*> measurement

runFuseHeight :: (Monad m, Floating a, Real a) => a -> KalmanState m a (a, a)
runFuseHeight = runFusion $ vecZ $ nedToVec3 $ fusePos <*> posNoise

runFuseTAS :: (Monad m, Floating a, Real a) => a -> KalmanState m a (a, a)
runFuseTAS = runFusion $ fuseTAS tasNoise

runFuseMag :: (Monad m, Floating a, Real a) => XYZ a -> KalmanState m a (XYZ (a, a))
runFuseMag measurement = sequence $ runFusion <$> (fuseMag <*> magNoise) <*> measurement
