{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.Estimator.Augment (
  AugmentState(..), augmentProcess
) where

import Control.Applicative
import Data.Distributive
import Data.Foldable
import Data.Traversable
import Linear
import Numeric.Estimator.Class

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

augmentProcess :: (Num (Var t), Applicative state, Applicative extra, Process t, GaussianFilter (Filter t), State t ~ AugmentState state extra)
               => t
               -> extra (Var t)
               -> state (state (Var t))
               -> extra (extra (Var t))
               -> Filter t state (Var t)
               -> Filter t state (Var t)
augmentProcess model extraState noise extraNoise prior = posterior
  where
  augmentedNoise = augment2D (pure (pure 0)) noise
  augmentedPrior = mapStatistics (flip AugmentState extraState) (augment2D extraNoise) prior
  augmentedPosterior = process model augmentedNoise augmentedPrior
  posterior = mapStatistics getState (fmap getState . getState) augmentedPosterior
