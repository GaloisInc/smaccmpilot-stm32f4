module SMACCM.INS.Simulate where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import MonadLib (runStateT, StateT, get, set)
import Prelude hiding (mapM, sequence, sum)
import SMACCM.INS.Matrix (diagMat)
import SMACCM.INS.SensorFusionModel
import SMACCM.INS.Vec3

type KalmanState m a = StateT (a, StateVector a, StateVector (StateVector a)) m

runKalmanState :: (Monad m, Fractional a) => a -> StateVector a -> KalmanState m a b -> m (b, (a, StateVector a, StateVector (StateVector a)))
runKalmanState ts state = runStateT (ts, state, kalmanP)

fixQuat :: Floating a => StateVector a -> StateVector a
fixQuat state = (pure id) { stateOrient = pure (/ quatMag) } <*> state
    where
    quatMag = sqrt $ sum $ fmap (^ (2 :: Int)) $ stateOrient state

runProcessModel :: (Monad m, Floating a, Ord a) => a -> DisturbanceVector a -> KalmanState m a ()
runProcessModel dt dist = do
    (ts, state, p) <- get
    let speed = sqrt $ sum $ fmap (^ (2 :: Int)) $ stateVel state
    let noise = if speed < 4 then (processNoise dt) { stateWind = pure 0, stateMagNED = pure 0, stateMagXYZ = pure 0 } else processNoise dt
    let (state', p') = updateProcess dt state dist p $ diagMat noise
    set (ts, fixQuat state', p')

runFusion :: (Monad m, Floating a, Ord a) => (a -> StateVector a -> StateVector (StateVector a) -> (a, a, StateVector a, StateVector (StateVector a))) -> a -> KalmanState m a (a, a)
runFusion fuse measurement = do
    (ts, state, p) <- get
    let (innov, innovCov, state', p') = fuse measurement state p
    let speed = sqrt $ sum $ fmap (^ (2 :: Int)) $ stateVel state
    let whenFlying = if speed < 4 then \ _new old -> old else \ new _old -> new
    let observable = (pure const) { stateWind = pure whenFlying, stateMagNED = pure whenFlying, stateMagXYZ = pure whenFlying }
    set (ts, observable <*> fixQuat state' <*> state, observable <*> p' <*> p)
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
