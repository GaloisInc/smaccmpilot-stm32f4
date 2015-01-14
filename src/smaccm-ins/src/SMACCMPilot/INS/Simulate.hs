{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.INS.Simulate where

import Control.Applicative
import Control.Lens ((^.))
import Data.Reflection (Reifies)
import Data.Traversable
import Linear
import MonadLib (runStateT, StateT, get, set)
import Numeric.AD
import Numeric.AD.Internal.Reverse
import Numeric.Estimator
import Numeric.Estimator.Model.Coordinate
import Numeric.Estimator.Model.SensorFusion
import Prelude hiding (mapM, sequence, sum)

type KalmanState m a = StateT (a, KalmanFilter StateVector a) m

runKalmanState :: (Monad m, Fractional a) => a -> StateVector a -> KalmanState m a b -> m (b, (a, KalmanFilter StateVector a))
runKalmanState ts state = runStateT (ts, KalmanFilter state initCovariance)

fixQuat :: Floating a => StateVector a -> StateVector a
fixQuat state = state { stateOrient = signorm $ stateOrient state }

runProcessModel :: (Monad m, Floating a, Ord a) => a -> StateVector a -> DisturbanceVector a -> DisturbanceVector a -> KalmanState m a ()
runProcessModel dt noise distNoise dist = do
    (ts, prior) <- get
    let speed = norm $ stateVel $ kalmanState prior
    let noise' = if speed < 4 then noise { stateWind = pure 0, stateMagNED = pure 0, stateMagXYZ = pure 0 } else noise
    let KalmanFilter state' p' = augmentProcess (EKFProcess $ processModel $ auto dt) dist (kronecker noise') (kronecker distNoise) prior
    set (ts, KalmanFilter (fixQuat state') p')

runFusion :: (Monad m, Floating a, Ord a) => (a -> KalmanFilter StateVector a -> (a, a, KalmanFilter StateVector a)) -> a -> KalmanState m a (a, a)
runFusion fuse measurement = do
    (ts, prior) <- get
    let (innov, innovCov, KalmanFilter state' p') = fuse measurement prior
    let speed = norm $ stateVel $ kalmanState prior
    let whenFlying :: v -> v -> v
        whenFlying = if speed < 4 then \ _new old -> old else \ new _old -> new
    let observable = (pure const) { stateWind = pure whenFlying, stateMagNED = pure whenFlying, stateMagXYZ = pure whenFlying }
    set (ts, KalmanFilter (observable <*> fixQuat state' <*> kalmanState prior) (observable <*> p' <*> kalmanCovariance prior))
    return (innov, innovCov)

-- A Fusion is a function from measurement covariance and measurement to
-- innovation, innovation covariance, new state, and new estimated state
-- covariance.
--
-- This version only supports scalar measurements. It's useful for sequential
-- fusion. It's also useful for partial measurements, such as measuring only
-- altitude when you've modeled 3D position.
type Fusion t = Var t -> Var t -> Filter t (State t) (Var t) -> (Var t, Var t, Filter t (State t) (Var t))
fusion :: (Measure t, MeasureObservable t V1, MeasureQuality t V1 ~ KalmanInnovation V1 (Var t)) => t -> Fusion t
fusion v cov m prior = let (KalmanInnovation (V1 innov) (V1 (V1 innovCov)), posterior) = measure (V1 (m, v)) (V1 (V1 cov)) prior in (innov, innovCov, posterior)

fuseV3 :: Fractional a => (forall s. Reifies s Tape => StateVector (Reverse s a) -> V3 (Reverse s a)) -> V3 (Fusion (EKFMeasurement StateVector a))
fuseV3 getV = fusion <$> V3 (EKFMeasurement $ (^._x) . getV) (EKFMeasurement $ (^._y) . getV) (EKFMeasurement $ (^._z) . getV)

fuseVel :: Fractional var => NED (Fusion (EKFMeasurement StateVector var))
fuseVel = NED $ fuseV3 $ nedToVec3 . stateVel

fusePos :: Fractional var => NED (Fusion (EKFMeasurement StateVector var))
fusePos = NED $ fuseV3 $ nedToVec3 . statePos

fusePressure :: Floating var => Fusion (EKFMeasurement StateVector var)
fusePressure = fusion $ EKFMeasurement statePressure

fuseTAS :: Floating var => Fusion (EKFMeasurement StateVector var)
fuseTAS = fusion $ EKFMeasurement stateTAS

fuseMag :: Fractional var => XYZ (Fusion (EKFMeasurement StateVector var))
fuseMag = XYZ $ fuseV3 $ xyzToVec3 . stateMag

runFuseVel :: (Monad m, Floating a, Real a) => NED a -> NED a -> KalmanState m a (NED (a, a))
runFuseVel noise measurement = sequence $ runFusion <$> (fuseVel <*> noise) <*> measurement

runFusePos :: (Monad m, Floating a, Real a) => NED a -> NED a -> KalmanState m a (NED (a, a))
runFusePos noise measurement = sequence $ runFusion <$> (fusePos <*> noise) <*> measurement

runFuseHeight :: (Monad m, Floating a, Real a) => a -> a -> KalmanState m a (a, a)
runFuseHeight noise = runFusion $ (nedToVec3 fusePos ^._z) noise

runFusePressure :: (Monad m, Floating a, Real a) => a -> a -> KalmanState m a (a, a)
runFusePressure noise = runFusion $ fusePressure noise

runFuseTAS :: (Monad m, Floating a, Real a) => a -> a -> KalmanState m a (a, a)
runFuseTAS noise = runFusion $ fuseTAS noise

runFuseMag :: (Monad m, Floating a, Real a) => XYZ a -> XYZ a -> KalmanState m a (XYZ (a, a))
runFuseMag noise measurement = sequence $ runFusion <$> (fuseMag <*> noise) <*> measurement
