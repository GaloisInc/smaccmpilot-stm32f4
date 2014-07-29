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

diagMat :: Num a => [a] -> [[a]]
diagMat = foldr push []
    where
    push v [] = [[v]]
    push v rs@(r:_) = (v : map (const 0) r) : map (0 :) rs

msplit :: [[a]] -> (a, [[a]], [[a]], [[a]])
msplit (row:rows) = (first, [top], left, rest)
    where
    first : top = row
    (left, rest) = unzip $ map (\(x:xs)-> ([x], xs)) rows

mjoin :: (a, [[a]], [[a]], [[a]]) -> [[a]]
mjoin (first, top, left, rest) = ([[first]] `hcat` top) ++ (left `hcat` rest)
    where hcat = zipWith (++)

matInvert :: Fractional a => [[a]] -> [[a]]
matInvert [] = []
matInvert m = mjoin (a', b', c', d')
    where
    (a, b, c, d) = msplit m
    aInv = recip a
    caInv = matUnOp (* aInv) c
    aInvb = matUnOp (aInv *) b
    d' = matInvert $ matBinOp (-) d $ matMult c aInvb
    c' = matUnOp negate $ matMult d' caInv
    b' = matUnOp negate $ matMult aInvb d'
    a' = aInv + case matMult aInvb $ matMult d' caInv of
        [[]] -> 0
        [[v]] -> v
