{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module SMACCMPilot.INS.Attitude.Ivory (
  kalmanInit, kalmanPredict,
  magMeasure, accelMeasure,
  stateVectorFromStruct, covarianceFromStruct,

  module SMACCMPilot.INS.Attitude.Types
) where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Ivory.Language
import Ivory.Stdlib
import Linear
import Numeric.AD
import Numeric.Estimator
import Numeric.Estimator.Model.Coordinate
import Numeric.Estimator.Model.Symbolic
import Prelude hiding (mapM, sequence_)
import SMACCMPilot.INS.Attitude.Parameters
import SMACCMPilot.INS.Attitude.SensorFusion
import SMACCMPilot.INS.Attitude.Simulate
import SMACCMPilot.INS.Attitude.Types

instance HasAtan2 IFloat where
  arctan2 = atan2F

vec3FromArray :: ( IvoryRef ref
                 , IvoryExpr (ref s v)
                 , IvoryExpr (ref s ('Array 3 v))
                 , IvoryArea v)
              => V3 ((ref s t -> ref s ('Array 3 v)) -> ref s t -> ref s v)
vec3FromArray = V3 ((! 0) .) ((! 1) .) ((! 2) .)

stateVectorFromStruct :: ( IvoryRef ref
                         , IvoryExpr (ref s ('Stored IFloat))
                         , IvoryExpr (ref s ('Array 3 ('Stored IFloat)))
                         , IvoryExpr (ref s ('Array 4 ('Stored IFloat)))
                         , IvoryExpr (ref s ('Struct "att_kalman_state")))
                      => ref s ('Struct "att_kalman_state")
                      -> StateVector (ref s ('Stored IFloat))
stateVectorFromStruct s = StateVector
  { stateOrient = Quaternion ((! 0) .) (V3 ((! 1) .) ((! 2) .) ((! 3) .)) <*> pure (~> orient)
  , stateMagNED = NED vec3FromArray <*> pure (~> mag_ned)
  } <*> pure s

covarianceFromStruct :: ( IvoryRef ref
                        , IvoryExpr (ref s ('Stored IFloat))
                        , IvoryExpr (ref s ('Array 3 ('Stored IFloat)))
                        , IvoryExpr (ref s ('Array 4 ('Stored IFloat)))
                        , IvoryExpr (ref s ('Struct "att_kalman_state"))
                        , IvoryExpr (ref s ('Array 3 ('Struct "att_kalman_state")))
                        , IvoryExpr (ref s ('Array 4 ('Struct "att_kalman_state")))
                        , IvoryExpr (ref s ('Struct "att_kalman_covariance")))
                     => ref s ('Struct "att_kalman_covariance")
                     -> StateVector (StateVector (ref s ('Stored IFloat)))
covarianceFromStruct s = stateVectorFromStruct <$> (StateVector
  { stateOrient = Quaternion ((! 0) .) (V3 ((! 1) .) ((! 2) .) ((! 3) .)) <*> pure (~> cov_orient)
  , stateMagNED = NED vec3FromArray <*> pure (~> cov_mag_ned)
  } <*> pure s)

storeRow :: (Applicative f, Foldable f, IvoryStore a, IvoryExpr a) => f (Ref s ('Stored a)) -> f a -> Ivory eff ()
storeRow vars vals = sequence_ $ liftA2 store vars vals

kalmanInit :: Ref s1 ('Struct "att_kalman_state") -> Ref s2 ('Struct "att_kalman_covariance") -> XYZ IFloat -> XYZ IFloat -> Ivory eff ()
kalmanInit state_ptr cov_ptr acc mag = do
  let stateVector = stateVectorFromStruct state_ptr
  let covariance = covarianceFromStruct cov_ptr
  let initialState = initDynamic acc mag 0
  storeRow stateVector initialState
  sequence_ $ liftA2 storeRow covariance initCovariance

kalmanPredict :: Ref s1 ('Struct "att_kalman_state") -> Ref s2 ('Struct "att_kalman_covariance") -> IFloat -> XYZ IFloat -> Ivory eff ()
kalmanPredict state_ptr cov_ptr dt gyro = do
  let stateVector = stateVectorFromStruct state_ptr
  let covariance = covarianceFromStruct cov_ptr
  stateVectorTemp <- mapM deref stateVector
  pTemp <- mapM (mapM deref) covariance
  let noise = processNoise dt
  let KalmanFilter stateVector' covariance' = augmentProcess (EKFProcess $ processModel $ auto dt) gyro (scaled noise) (scaled gyroNoise) $ KalmanFilter stateVectorTemp pTemp
  storeRow stateVector $ fixQuat stateVector'
  sequence_ $ liftA2 storeRow covariance covariance'

applyUpdate :: Ref s1 ('Struct "att_kalman_state") -> Ref s2 ('Struct "att_kalman_covariance") -> IFloat -> (KalmanFilter StateVector IFloat -> (IFloat, IFloat, KalmanFilter StateVector IFloat)) -> Ivory eff ()
applyUpdate state_ptr cov_ptr cov fusionStep = do
    let stateVector = stateVectorFromStruct state_ptr
    let covariance = covarianceFromStruct cov_ptr
    stateVectorTemp <- mapM deref stateVector
    pTemp <- mapM (mapM deref) covariance
    let (innov, innovCov, KalmanFilter stateVector' covariance') = fusionStep $ KalmanFilter stateVectorTemp pTemp
    let forceUpdate = true -- filter state is not yet right; muddle through anyway
    -- TODO: when innovCov < cov, add cov to the "right" elements of covariance
    when (forceUpdate .|| innovCov >=? cov) $ do
      when (forceUpdate .|| innov ^ (2 :: Int) / innovCov <? 5 ^ (2 :: Int)) $ do
        storeRow stateVector $ fixQuat stateVector'
        sequence_ $ liftA2 storeRow covariance covariance'

magMeasure :: Ref s1 ('Struct "att_kalman_state") -> Ref s2 ('Struct "att_kalman_covariance") -> XYZ IFloat -> Ivory eff ()
magMeasure state_ptr cov_ptr mag = sequence_ $ applyUpdate state_ptr cov_ptr <$> magNoise <*> (fuseMag <*> magNoise <*> mag)

accelMeasure :: Ref s1 ('Struct "att_kalman_state") -> Ref s2 ('Struct "att_kalman_covariance") -> XYZ IFloat -> Ivory eff ()
accelMeasure state_ptr cov_ptr accel = sequence_ $ applyUpdate state_ptr cov_ptr <$> accelNoise <*> (fuseAccel <*> accelNoise <*> accel)
