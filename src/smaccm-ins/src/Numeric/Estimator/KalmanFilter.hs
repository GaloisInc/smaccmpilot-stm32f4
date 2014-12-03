{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.Estimator.KalmanFilter where

import Data.Distributive
import Data.Reflection (Reifies)
import Data.Traversable
import Linear
import Numeric.AD.Internal.Reverse (Tape)
import Numeric.AD.Mode.Reverse
import Numeric.Estimator.Class
import Numeric.Estimator.Matrix

data KalmanFilter state var = KalmanFilter
  { kalmanState :: state var
  , kalmanCovariance :: state (state var)
  }

type instance State (KalmanFilter state var) = state
type instance Var (KalmanFilter state var) = var

instance GaussianFilter KalmanFilter where
  mapStatistics mapState mapCov (KalmanFilter state cov) = KalmanFilter (mapState state) (mapCov cov)

data KalmanInnovation obs var = KalmanInnovation
  { kalmanInnovation :: obs var
  , kalmanInnovationCovariance :: obs (obs var)
  }

newtype EKFProcess state var = EKFProcess (forall s. Reifies s Tape => state (Reverse s var) -> state (Reverse s var))

type instance State (EKFProcess state var) = state
type instance Var (EKFProcess state var) = var

instance Estimator (EKFProcess state var) where
  type Filter (EKFProcess state var) = KalmanFilter

instance (Additive state, Traversable state, Distributive state, Num var) => Process (EKFProcess state var) where
  process (EKFProcess model) q prior = KalmanFilter state' (q !+! f !*! kalmanCovariance prior !*! transpose f)
    where
    predicted = jacobian' model $ kalmanState prior
    state' = fmap fst predicted
    f = fmap snd predicted

newtype EKFMeasurement state var = EKFMeasurement (forall s. Reifies s Tape => state (Reverse s var) -> Reverse s var)

type instance State (EKFMeasurement state var) = state
type instance Var (EKFMeasurement state var) = var

instance Estimator (EKFMeasurement state var) where
  type Filter (EKFMeasurement state var) = KalmanFilter

instance (Additive state, Distributive state, Traversable state, Fractional var) => Measure (EKFMeasurement state var) where
  type MeasureQuality (EKFMeasurement state var) obs = KalmanInnovation obs var
  type MeasureObservable (EKFMeasurement state var) obs = (Additive obs, Traversable obs)

  measure measurements obsCov prior = (KalmanInnovation innovation innovCov, KalmanFilter state' errorCov')
    where
    predicted = jacobian' (\ stateVars -> fmap (\ (_, EKFMeasurement h) -> h stateVars) measurements) (kalmanState prior)
    innovation = fmap fst measurements ^-^ fmap fst predicted
    obsModel = fmap snd predicted
    ph = kalmanCovariance prior !*! transpose obsModel
    innovCov = obsCov !+! obsModel !*! ph
    obsGain = ph !*! matInvert innovCov
    errorCov' = kalmanCovariance prior !-! obsGain !*! obsModel !*! kalmanCovariance prior
    state' = kalmanState prior ^+^ (obsGain !* innovation)
