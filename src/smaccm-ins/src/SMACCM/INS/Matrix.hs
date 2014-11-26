{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SMACCM.INS.Matrix where

import Control.Applicative
import Data.Distributive
import Data.Foldable
import Data.Traversable
import Prelude hiding (foldl, foldr, sum)

-- The class of applicative functors that combine elements point by
-- point. For lists, that would mean that liftA2 == zipWith; however,
-- lists' Applicative instance actually makes liftA2 == liftM2, which
-- is defined in terms of concatMap. In general, if a type has a Monad
-- instance, its Applicative instance is not Pointwise.
class Applicative f => Pointwise f where

matUnOp :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
matUnOp f = fmap (fmap f)

matBinOp :: (Pointwise f, Pointwise g) => (a -> b -> c) -> f (g a) -> f (g b) -> f (g c)
matBinOp f = liftA2 (liftA2 f)

dotp :: (Pointwise f, Foldable f, Num a) => f a -> f a -> a
dotp a b = sum $ liftA2 (*) a b

matMult :: (Functor f, Pointwise g, Foldable g, Pointwise h, Num a) => f (g a) -> g (h a) -> f (h a)
matMult a b = fmap (\ row -> vecMatMult row b) a

matVecMult :: (Functor f, Pointwise g, Foldable g, Num a) => f (g a) -> g a -> f a
matVecMult m v = fmap (dotp v) m

vecMatMult :: (Pointwise f, Foldable f, Pointwise g, Num a) => f a -> f (g a) -> g a
vecMatMult v m = addRows $ mulRows m v
    where
    addRows = foldl (liftA2 (+)) (pure 0)
    mulRows rhs row = (\ cell -> fmap (cell *)) <$> row <*> rhs

diagMat :: (Traversable f, Num a) => f a -> f (f a)
diagMat vec = countUp
    where
    countUp = snd $ mapAccumL (\ count _ -> (count + 1, countDown count)) (0 :: Int) vec
    countDown from = snd $ mapAccumL (\ count v -> (count - 1, if count == 0 then v else 0)) from vec

matTranspose :: (Functor f, Distributive g) => f (g a) -> g (f a)
matTranspose = distribute

instance Pointwise ZipList
instance Foldable ZipList where
    foldr k z = foldr k z . getZipList

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
    d' = matInvertList $ matBinOp (-) d $ fmap (\ v -> fmap (v *) aInvb) c
    c' = fmap negate $ matVecMult d' caInv
    b' = fmap negate $ vecMatMult aInvb d'
    a' = aInv + dotp aInvb (matVecMult d' caInv)

copyInto :: Traversable f => f a -> ZipList a -> f a
copyInto structure (ZipList contents) = result
    where
    ([], result) = mapAccumL (\ (x:xs) _ -> (xs, x)) contents structure

matInvert :: (Traversable f, Fractional a) => f (f a) -> f (f a)
matInvert m = copyInto m $ liftA2 copyInto (toL m) $ matInvertList $ fmap toL $ toL m
    where
    toL = ZipList . toList
