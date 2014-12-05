{- |
Description: Re-export the common estimator modules

System models using this package will usually require these modules.
-}

module Numeric.Estimator (
  -- * Generic types
  module Numeric.Estimator.Class,
  -- * Implementation of the Kalman Filter family of algorithms
  module Numeric.Estimator.KalmanFilter,
  -- * Support for augmented process models
  module Numeric.Estimator.Augment
) where

import Numeric.Estimator.Augment
import Numeric.Estimator.Class
import Numeric.Estimator.KalmanFilter
