module Matrix where

import Data.List

matUnOp :: (a -> b) -> [[a]] -> [[b]]
matUnOp f = map (map f)

matBinOp :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
matBinOp f = zipWith (zipWith f)

dotp :: Num a => [a] -> [a] -> a
dotp a b = sum $ zipWith (*) a b

matMult :: Num a => [[a]] -> [[a]] -> [[a]]
matMult a b = [ [ dotp l r | r <- transpose b ] | l <- a ]

matVecMult :: Num a => [[a]] -> [a] -> [a]
matVecMult m v = [ dotp r v | r <- m ]

vecMatMult :: Num a => [a] -> [[a]] -> [a]
vecMatMult v m = [ dotp v c | c <- transpose m ]

diagMat :: Num a => [a] -> [[a]]
diagMat = foldr push []
    where
    push v [] = [[v]]
    push v rs@(r:_) = (v : map (const 0) r) : map (0 :) rs

msplit :: [[a]] -> (a, [a], [a], [[a]])
msplit (row:rows) = (first, top, left, rest)
    where
    first : top = row
    (left, rest) = unzip $ map (\(x:xs)-> (x, xs)) rows

mjoin :: (a, [a], [a], [[a]]) -> [[a]]
mjoin (first, top, left, rest) = (first : top) : (zipWith (:) left rest)

matInvert :: Fractional a => [[a]] -> [[a]]
matInvert [] = []
matInvert [[a]] = [[recip a]]
matInvert m = mjoin (a', b', c', d')
    where
    (a, b, c, d) = msplit m
    aInv = recip a
    caInv = fmap (* aInv) c
    aInvb = fmap (aInv *) b
    d' = matInvert $ matBinOp (-) d $ fmap (\ v -> fmap (v *) aInvb) c
    c' = fmap negate $ matVecMult d' caInv
    b' = fmap negate $ vecMatMult aInvb d'
    a' = aInv + dotp aInvb (matVecMult d' caInv)
