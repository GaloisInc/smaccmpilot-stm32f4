{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Description: Helper to augment a process model

Some system models are best handled by injecting some measurements into
the process model. These measurements are not truly part of the filter
state, and so shouldn't appear in the state vector. However, when the
process model runs, the state needs to be augmented with these
measurements, and the process uncertainty needs to be augmented with
their noise covariance.

As currently implemented, this only works for process models where the
'Filter' type is a 'GaussianFilter' instance. Generalizing this
interface would be useful future work.
-}

module Numeric.Estimator.Augment (
  AugmentState(..), augmentProcess
) where

import Control.Applicative
import Data.Distributive
import Data.Foldable
import Data.Traversable
import Linear
import Numeric.Estimator.Class

-- | Holder for the basic state vector plus the augmented extra state.
data AugmentState state extra a = AugmentState { getState :: state a, getExtra :: extra a }

instance (Applicative state, Applicative extra) => Additive (AugmentState state extra) where
  zero = pure 0

instance (Applicative state, Applicative extra) => Applicative (AugmentState state extra) where
  pure v = AugmentState (pure v) (pure v)
  v1 <*> v2 = AugmentState
    { getState = getState v1 <*> getState v2
    , getExtra = getExtra v1 <*> getExtra v2
    }

instance (Applicative state, Applicative extra) => Functor (AugmentState state extra) where
  fmap = liftA

instance (Applicative state, Applicative extra, Traversable state, Traversable extra) => Foldable (AugmentState state extra) where
  foldMap = foldMapDefault

instance (Applicative state, Applicative extra, Traversable state, Traversable extra) => Traversable (AugmentState state extra) where
  sequenceA v = AugmentState
    <$> sequenceA (getState v)
    <*> sequenceA (getExtra v)

instance (Applicative state, Applicative extra, Distributive state, Distributive extra) => Distributive (AugmentState state extra) where
  distribute f = AugmentState
    { getState = distribute $ fmap getState f
    , getExtra = distribute $ fmap getExtra f
    }

augment2D :: (Applicative state, Applicative extra, Num a) => extra (extra a) -> state (state a) -> AugmentState state extra (AugmentState state extra a)
augment2D lr ul = AugmentState (liftA2 AugmentState ul (pure (pure 0))) (liftA2 AugmentState (pure (pure 0)) lr)

-- | Run an augmented process model with the given extra data.
augmentProcess :: (Num (Var t), Applicative state, Applicative extra, Process t, GaussianFilter (Filter t), State t ~ AugmentState state extra)
               => t
               -- ^ base process model
               -> extra (Var t)
               -- ^ extra state
               -> state (state (Var t))
               -- ^ base process uncertainty
               -> extra (extra (Var t))
               -- ^ extra process uncertainty
               -> Filter t state (Var t)
               -- ^ prior (unaugmented) state
               -> Filter t state (Var t)
               -- ^ posterior (unaugmented) state
augmentProcess model extraState noise extraNoise prior = posterior
  where
  augmentedNoise = augment2D (pure (pure 0)) noise
  augmentedPrior = mapStatistics (flip AugmentState extraState) (augment2D extraNoise) prior
  augmentedPosterior = process model augmentedNoise augmentedPrior
  posterior = mapStatistics getState (fmap getState . getState) augmentedPosterior
