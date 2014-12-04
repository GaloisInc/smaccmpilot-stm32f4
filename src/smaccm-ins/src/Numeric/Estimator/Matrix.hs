{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Description: Matrix utilities

These functions extend the facilities provided by the 'Linear' module.
They should not be used by external code, but might be useful
contributions to the linear package.
-}

module Numeric.Estimator.Matrix (matInvert) where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Linear
import Prelude hiding (foldr)

instance Metric []

msplit :: [a] -> [[a]] -> (a, [a], [a], [[a]])
msplit row rows = (first, top, left, rest)
    where
    first : top = row
    (left, rest) = unzip $ map (\ (x:xs) -> (x, xs)) rows

mjoin :: (a, [a], [a], [[a]]) -> [[a]]
mjoin (first, top, left, rest) = (first : top) : (zipWith (\ l r -> l : r) left rest)

matInvertList :: Fractional a => [[a]] -> [[a]]
matInvertList [] = []
matInvertList [[a]] = [[recip a]]
matInvertList (row : rows) = mjoin (a', b', c', d')
    where
    (a, b, c, d) = msplit row rows
    aInv = recip a
    caInv = fmap (* aInv) c
    aInvb = fmap (aInv *) b
    d' = matInvertList $ d !-! outer c aInvb
    c' = negated $ d' !* caInv
    b' = negated $ aInvb *! d'
    a' = aInv + dot aInvb (d' !* caInv)

copyInto :: Traversable f => f a -> [a] -> f a
copyInto structure contents = snd $ mapAccumL (\ (x:xs) _ -> (xs, x)) contents structure

-- | Compute the matrix inverse of a square matrix.
matInvert :: (Traversable f, Fractional a) => f (f a) -> f (f a)
matInvert m = copyInto m $ liftA2 copyInto (toList m) $ matInvertList $ fmap toList $ toList m
