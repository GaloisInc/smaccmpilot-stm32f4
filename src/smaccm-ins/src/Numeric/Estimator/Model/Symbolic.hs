{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Description: Support for purely symbolic models

This package supports running filters in pure Haskell, of course. But
it's also designed to work with libraries like
<http://hackage.haskell.org/package/sbv sbv> and
<http://hackage.haskell.org/package/ivory ivory> that can extract
symbolic expressions, whether for analysis in other tools or for
generating code in some other language.

This module provides helpers allowing models to abstract away from
standard Haskell type-classes that do not support symbolic computation.
-}

module Numeric.Estimator.Model.Symbolic where

-- | The 'atan2' function is defined in the 'RealFloat' typeclass, which
-- can't be implemented for symbolic types because nearly every member
-- besides 'atan2' returns concrete values, not symbolic ones. This
-- typeclass describes types, symbolic or concrete, that support an
-- 'atan2' function.
class HasAtan2 a where
    -- | Another name for 'atan2', chosen not to collide with
    -- 'RealFloat'.
    arctan2 :: a -> a -> a

instance HasAtan2 Double where
    arctan2 = atan2
