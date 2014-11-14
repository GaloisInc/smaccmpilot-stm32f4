module SMACCM.INS.Simulate where

import Control.Applicative
import Data.Traversable
import MonadLib (runStateT, StateT, get, set)
import Prelude hiding (mapM, sequence, sum)
import SMACCM.INS.SensorFusionModel
import SMACCM.INS.Vec3

type KalmanState m a = StateT (a, StateVector a, StateVector (StateVector a)) m

runKalmanState :: (Monad m, Fractional a) => a -> StateVector a -> KalmanState m a b -> m (b, (a, StateVector a, StateVector (StateVector a)))
runKalmanState ts state = runStateT (ts, state, kalmanP)

fixQuat :: Floating a => StateVector a -> StateVector a
fixQuat state = (pure id) { stateOrient = pure (/ vecMag (stateOrient state)) } <*> state

runProcessModel :: (Monad m, Floating a, Ord a) => a -> StateVector a -> DisturbanceVector a -> DisturbanceVector a -> KalmanState m a ()
runProcessModel dt noise distNoise dist = do
    (ts, state, p) <- get
    let speed = vecMag $ stateVel state
    let noise' = if speed < 4 then noise { stateWind = pure 0, stateMagNED = pure 0, stateMagXYZ = pure 0 } else noise
    let (state', p') = updateProcess dt state dist p noise' distNoise
    set (ts, fixQuat state', p')

runFusion :: (Monad m, Floating a, Ord a) => (a -> StateVector a -> StateVector (StateVector a) -> (a, a, StateVector a, StateVector (StateVector a))) -> a -> KalmanState m a (a, a)
runFusion fuse measurement = do
    (ts, state, p) <- get
    let (innov, innovCov, state', p') = fuse measurement state p
    let speed = vecMag $ stateVel state
    let whenFlying = if speed < 4 then \ _new old -> old else \ new _old -> new
    let observable = (pure const) { stateWind = pure whenFlying, stateMagNED = pure whenFlying, stateMagXYZ = pure whenFlying }
    set (ts, observable <*> fixQuat state' <*> state, observable <*> p' <*> p)
    return (innov, innovCov)

runFuseVel :: (Monad m, Floating a, Real a) => NED a -> NED a -> KalmanState m a (NED (a, a))
runFuseVel noise measurement = sequence $ runFusion <$> (fuseVel <*> noise) <*> measurement

runFusePos :: (Monad m, Floating a, Real a) => NED a -> NED a -> KalmanState m a (NED (a, a))
runFusePos noise measurement = sequence $ runFusion <$> (fusePos <*> noise) <*> measurement

runFuseHeight :: (Monad m, Floating a, Real a) => a -> a -> KalmanState m a (a, a)
runFuseHeight noise = runFusion $ (vecZ $ nedToVec3 fusePos) noise

runFusePressure :: (Monad m, Floating a, Real a) => a -> a -> KalmanState m a (a, a)
runFusePressure noise = runFusion $ fusePressure noise

runFuseTAS :: (Monad m, Floating a, Real a) => a -> a -> KalmanState m a (a, a)
runFuseTAS noise = runFusion $ fuseTAS noise

runFuseMag :: (Monad m, Floating a, Real a) => XYZ a -> XYZ a -> KalmanState m a (XYZ (a, a))
runFuseMag noise measurement = sequence $ runFusion <$> (fuseMag <*> noise) <*> measurement
