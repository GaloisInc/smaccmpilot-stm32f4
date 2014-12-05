{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Description: Type-classes for state-space estimation algorithms

These type classes abstract many details of estimation algorithms,
making it easier to try different algorithms while changing the model as
little as possible.

This interface does make the simplifying assumption that process
uncertainty and measurement noise are each always specified as a
covariance matrix describing a zero-mean multi-variate normal
distribution. While some estimation algorithms (such as the Bayesian
Particle Filter) can accomodate more sophisticated distributions, it's
unusual to encounter problems that require that degree of flexibility.
-}

module Numeric.Estimator.Class where

-- | An estimator is a model of a system, describing how to update a
-- prior estimated state with new information. Two kinds of estimators
-- are the 'Process' model, and the 'Measure' (or observation) model.
class Estimator t where
  -- | The type of data that this estimator maintains across updates.
  type Filter t :: (* -> *) -> * -> *

-- | The type of state vector used in an estimator.
type family State t :: * -> *

-- | The type of individual state variables used in an estimator.
type family Var t

-- | A process model updates the estimated state by predicting how the
-- system should have changed since the last prediction.
--
-- In a kinematic model, for instance, the process model might be a
-- dead-reckoning physics simulation which updates position using a
-- trivial numeric integration of velocity.
--
-- Parameter estimation problems, where the parameters are expected to
-- remain constant between observations, needn't have a process model.
class Estimator t => Process t where
  process :: t
          -- ^ process model
          -> State t (State t (Var t))
          -- ^ process uncertainty covariance
          -> Filter t (State t) (Var t)
          -- ^ prior state
          -> Filter t (State t) (Var t)
          -- ^ posterior state

-- | A measurement, or observation, model updates the estimated state
-- using some observation of the real state.
--
-- In a navigation problem, for instance, an observation might come from
-- a GPS receiver or a pressure altimeter. The model computes what value
-- the sensor would be expected to read if there were no sensor noise
-- and the current estimated state were exactly correct. The difference
-- between the expected and actual measurement is called the
-- \"innovation\", and that difference drives the estimated state toward
-- the true state.
--
-- In general, an observation is vector-valued. You can wrap up scalar
-- observations in a singleton functor, such as 'V1'.
--
-- For each dimension of the observation vector, the measurement must
-- consist of a scalar measurement, and an expression which evaluates to
-- the expected value for that measurement given the current state.
class Estimator t => Measure t where
  -- | Some estimators can compute some indication of how plausible an
  -- observation is, such as, for example, the innovation. This is the
  -- type of that quality indication, which may be @()@ if the chosen
  -- algorithm can't report measurement quality.
  type MeasureQuality t obs

  -- | An algorithm may have specific constraints on what types of
  -- observation it can process. This type has a 'Constraint' kind and
  -- captures any required type-class constraints.
  type MeasureObservable t obs

  measure :: MeasureObservable t obs
          => obs (Var t, t)
          -- ^ measurement model
          -> obs (obs (Var t))
          -- ^ measurement noise covariance
          -> Filter t (State t) (Var t)
          -- ^ prior state
          -> (MeasureQuality t obs, Filter t (State t) (Var t))
          -- ^ measurement quality and posterior state

-- | A filter whose state can be captured as a multi-variate normal
-- distribution can also be updated by adjusting the parameters of that
-- distribution.
class GaussianFilter t where
  mapStatistics :: (state var -> state' var')
                -- ^ update mean
                -> (state (state var) -> state' (state' var'))
                -- ^ update covariance
                -> t state var
                -- ^ original state
                -> t state' var'
                -- ^ updated state
