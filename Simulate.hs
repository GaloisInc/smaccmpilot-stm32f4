module Simulate where

import ExtendedKalmanFilter
import Matrix
import SensorFusionModel
import Quat
import SymDiff
import Vec3

import Control.Applicative
import Data.Foldable
import Data.Traversable
import MonadLib (runId, Id, runStateT, StateT, get, set, sets)
import Prelude hiding (mapM, sequence, sum)

kalmanP :: Fractional a => [[a]]
kalmanP = diagMat $ toList $ fmap (^ 2) $ StateVector
    { stateOrient = Quat (0.5, 0.5, 0.5, 5)
    , stateVel = pure 0.7
    , statePos = ned 15 15 5
    , stateGyroBias = pure $ 0.1 * deg2rad * dtIMU
    , stateWind = pure 8
    , stateMagNED = pure 0.02
    , stateMagXYZ = pure 0.02
    }
    where
    deg2rad = realToFrac (pi :: Double) / 180
    dtIMU = 0.1 -- FIXME: get dt from caller

initAttitude :: RealFloat a => XYZ a -> XYZ a -> a -> Quat a
initAttitude (XYZ accel) (XYZ mag) declination = heading * pitch * roll
    where
    initialRoll = atan2 (negate (vecY accel)) (negate (vecZ accel))
    initialPitch = atan2 (vecX accel) (negate (vecZ accel))
    magX = (vecX mag) * cos initialPitch + (vecY mag) * sin initialRoll * sin initialPitch + (vecZ mag) * cos initialRoll * sin initialPitch
    magY = (vecY mag) * cos initialRoll - (vecZ mag) * sin initialRoll
    initialHdg = atan2 (negate magY) magX + declination
    roll = Quat (cos (initialRoll / 2), sin (initialRoll / 2), 0, 0)
    pitch = Quat (cos (initialPitch / 2), 0, sin (initialPitch / 2), 0)
    heading = Quat (cos (initialHdg / 2), 0, 0, sin (initialHdg / 2))

initDynamic :: RealFloat a => XYZ a -> XYZ a -> XYZ a -> a -> NED a -> StateVector a
initDynamic accel mag magBias declination vel = (pure 0)
    { stateOrient = initQuat
    , stateVel = vel
    , stateMagNED = initMagNED
    , stateMagXYZ = magBias
    }
    where
    initMagXYZ = mag - magBias
    initQuat = initAttitude accel initMagXYZ declination
    initMagNED = let [n, e, d] = matVecMult (quatRotation initQuat) (toList initMagXYZ) in ned n e d
    -- TODO: re-implement InertialNav's calcEarthRateNED

gyroProcessNoise, accelProcessNoise :: Fractional a => a
gyroProcessNoise = 1.4544411e-2
accelProcessNoise = 0.5

distCovariance :: Fractional a => a -> DisturbanceVector a
distCovariance dt = DisturbanceVector
    { disturbanceGyro = pure ((dt * gyroProcessNoise) ^ 2)
    , disturbanceAccel = pure ((dt * accelProcessNoise) ^ 2)
    }

type KalmanState m a = StateT (a, StateVector a, [[a]]) m

runKalmanState :: (Monad m, Fractional a) => a -> StateVector a -> KalmanState m a b -> m (b, (a, StateVector a, [[a]]))
runKalmanState ts state = runStateT (ts, state, kalmanP)

type Uniq a = StateT Int Id a
getUniq :: Uniq Int
getUniq = sets (\ x -> (x, x + 1))
runUniq :: Uniq a -> a
runUniq = fst . runId . runStateT 0

fixQuat :: Floating a => StateVector a -> StateVector a
fixQuat state = (pure id) { stateOrient = pure (/ quatMag) } <*> state
    where
    quatMag = sqrt $ sum $ fmap (^ 2) $ stateOrient state

runProcessModel :: (Monad m, Floating a, Real a) => a -> DisturbanceVector a -> KalmanState m a ()
runProcessModel = \ dt dist -> do
    (ts, state, p) <- get
    let state' = processModel dt state dist
    let getValue idx = (dt : toList dist ++ toList state) !! idx
    let p' = map (map (eval . fmap getValue)) $ updateUniq $ map (map realToFrac) p
    set (ts, fixQuat state', p')
    where
    (dtUniq, distUniq, stateUniq) = runUniq $ (,,) <$> getUniq <*> mapM (const getUniq) (pure ()) <*> mapM (const getUniq) (pure ())
    updateUniq = kalmanPredict (processModel (var dtUniq)) stateUniq distUniq (distCovariance (var dtUniq))

runFusion :: (Monad m, Floating a, Real a) => (Int -> StateVector Int -> [[Sym Int]] -> (Sym Int, Sym Int, StateVector (Sym Int), [[Sym Int]])) -> a -> KalmanState m a (a, a)
runFusion fuse = \ measurement -> do
    (ts, state, p) <- get
    let getValue idx = (measurement : toList state) !! idx
    let (innov, innovCov, state', p') = updateUniq $ map (map realToFrac) p
    set (ts, fixQuat $ fmap (eval . fmap getValue) state', map (map (eval . fmap getValue)) p')
    return (eval $ fmap getValue innov, eval $ fmap getValue innovCov)
    where
    (measurementUniq, stateUniq) = runUniq $ (,) <$> getUniq <*> mapM (const getUniq) (pure ())
    updateUniq = fuse measurementUniq stateUniq

runFuseVel :: (Monad m, Floating a, Real a) => NED a -> KalmanState m a (NED (a, a))
runFuseVel measurement = sequence $ runFusion <$> (fuseVel <*> ned 0.04 0.04 0.08) <*> measurement

runFusePos :: (Monad m, Floating a, Real a) => NED a -> KalmanState m a (NED (a, a))
runFusePos measurement = sequence $ runFusion <$> (fusePos <*> pure 4) <*> measurement

runFuseHeight :: (Monad m, Floating a, Real a) => a -> KalmanState m a (a, a)
runFuseHeight = runFusion $ (vecZ $ nedToVec3 $ fusePos) 4

runFuseTAS :: (Monad m, Floating a, Real a) => a -> KalmanState m a (a, a)
runFuseTAS = runFusion $ fuseTAS 2

runFuseMag :: (Monad m, Floating a, Real a) => XYZ a -> KalmanState m a (XYZ (a, a))
runFuseMag measurement = sequence $ runFusion <$> (fuseMag <*> pure 0.0025) <*> measurement
