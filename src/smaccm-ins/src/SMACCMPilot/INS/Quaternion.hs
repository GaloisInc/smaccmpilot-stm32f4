{- |
Description: Quaternion utilities

These functions extend the facilities provided by the 'Linear' module.
They should not be used by external code, but might be useful
contributions to the linear package.
-}

module SMACCMPilot.INS.Quaternion where

import Control.Applicative
import Control.Lens
import Data.Foldable
import Linear
import Prelude hiding (foldl1, foldr, sum)

{- |
The Taylor series expansion of the quaternion axis-angle formula never
divides by any quantity that might be zero. It also avoids computing
fancy floating-point functions like sin, cos, or sqrt. And since non-x86
CPUs typically don't have those fancy functions in hardware, on those
platforms this implementation is as efficient as we're going to get and
allows us to control the tradeoff between computation time and accuracy
of the result.
-}
approxAxisAngle :: Fractional a => Int -> V3 a -> Quaternion a
approxAxisAngle order rotation = Quaternion c $ fmap (s *) rotation
    where
    halfSigmaSq = 0.25 * sum (fmap (^ (2 :: Int)) rotation)
    go prev idx = let cosTerm = prev / fromIntegral (negate idx); sinTerm = cosTerm / fromIntegral (idx + 1) in cosTerm : sinTerm : go (sinTerm * halfSigmaSq) (idx + 2)
    combine term (l, r) = (r + term, l)
    (c, s2) = foldr combine (1, 1) $ take (order - 1) $ go halfSigmaSq (2 :: Int)
    s = 0.5 * s2

{- |
Linear's 'Num' instance for 'Quaternion' requires 'RealFloat' in order
to implement 'signum', but we don't want to require 'RealFloat' as it
doesn't work for symbolic types. This is a copy of just the '*'
implementation, which only needed 'Num'.
-}
quatMul :: Num a => Quaternion a -> Quaternion a -> Quaternion a
quatMul (Quaternion s1 v1) (Quaternion s2 v2)
    = Quaternion (s1 * s2 - (v1 `dot` v2)) $ (v1 `cross` v2) + s1 *^ v2 + s2 *^ v1

fromEuler :: Floating a => a -> a -> a -> Quaternion a
fromEuler roll pitch yaw = foldl1 quatMul $ map (uncurry rotateAround)
    [ (ez, yaw)
    , (ey, pitch)
    , (ex, roll)
    ]
    where
    rotateAround axis theta = Quaternion (cos half) $ pure 0 & el axis .~ (sin half) where half = theta / 2
