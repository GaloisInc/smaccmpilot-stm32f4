{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SMACCM.INS.Matrix (matInvert) where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Linear
import Prelude hiding (foldr)

instance Foldable ZipList where
    foldr k z = foldr k z . getZipList

instance Metric ZipList

msplit :: [a] -> [ZipList a] -> (a, ZipList a, ZipList a, ZipList (ZipList a))
msplit row rows = (first, ZipList top, ZipList left, ZipList $ map ZipList rest)
    where
    first : top = row
    (left, rest) = unzip $ map (\ (ZipList (x:xs)) -> (x, xs)) rows

mjoin :: (a, ZipList a, ZipList a, ZipList (ZipList a)) -> ZipList (ZipList a)
mjoin (first, ZipList top, ZipList left, ZipList rest) = ZipList $ (ZipList $ first : top) : (zipWith (\ l (ZipList r) -> ZipList $ l : r) left rest)

matInvertList :: Fractional a => ZipList (ZipList a) -> ZipList (ZipList a)
matInvertList (ZipList []) = ZipList []
matInvertList (ZipList [ZipList [a]]) = ZipList [ZipList [recip a]]
matInvertList (ZipList (ZipList row : rows)) = mjoin (a', b', c', d')
    where
    (a, b, c, d) = msplit row rows
    aInv = recip a
    caInv = fmap (* aInv) c
    aInvb = fmap (aInv *) b
    d' = matInvertList $ d !-! outer c aInvb
    c' = negated $ d' !* caInv
    b' = negated $ aInvb *! d'
    a' = aInv + dot aInvb (d' !* caInv)

copyInto :: Traversable f => f a -> ZipList a -> f a
copyInto structure (ZipList contents) = result
    where
    ([], result) = mapAccumL (\ (x:xs) _ -> (xs, x)) contents structure

matInvert :: (Traversable f, Fractional a) => f (f a) -> f (f a)
matInvert m = copyInto m $ liftA2 copyInto (toL m) $ matInvertList $ fmap toL $ toL m
    where
    toL = ZipList . toList
