{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.Estimator.Class where

type family State t :: * -> *
type family Var t

class Estimator t where
  type Filter t :: (* -> *) -> * -> *

class Estimator t => Process t where
  process :: t -> State t (State t (Var t)) -> Filter t (State t) (Var t) -> Filter t (State t) (Var t)

class Estimator t => Measure t where
  type MeasureQuality t obs
  type MeasureObservable t obs

  measure :: MeasureObservable t obs => obs (Var t, t) -> obs (obs (Var t)) -> Filter t (State t) (Var t) -> (MeasureQuality t obs, Filter t (State t) (Var t))

class GaussianFilter t where
  mapStatistics :: (state var -> state' var') -> (state (state var) -> state' (state' var')) -> t state var -> t state' var'
