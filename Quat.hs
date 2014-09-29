module Quat where

import Control.Applicative
import Data.Distributive
import Data.Foldable
import Data.Traversable
import Vec3

newtype Quat a = Quat (a, a, a, a)
    deriving (Eq, Show)

instance Applicative Quat where
    pure v = Quat (v, v, v, v)
    (Quat (a1, b1, c1, d1)) <*> (Quat (a2, b2, c2, d2)) = Quat (a1 a2, b1 b2, c1 c2, d1 d2)

instance Functor Quat where
    fmap = liftA

instance Foldable Quat where
    foldMap = foldMapDefault

instance Traversable Quat where
    sequenceA (Quat (fa, fb, fc, fd)) = Quat <$> ((,,,) <$> fa <*> fb <*> fc <*> fd)

instance Distributive Quat where
    distribute f = Quat ((fmap (\ (Quat (v, _, _, _)) -> v) f), (fmap (\ (Quat (_, v, _, _)) -> v) f), (fmap (\ (Quat (_, _, v, _)) -> v) f), (fmap (\ (Quat (_, _, _, v)) -> v) f))

instance Num a => Num (Quat a) where
    q1 + q2 = (+) <$> q1 <*> q2
    negate = fmap negate

    (Quat (a1, b1, c1, d1)) * (Quat (a2, b2, c2, d2)) = Quat (
            a1 * a2 - b1 * b2 - c1 * c2 - d1 * d2,
            a1 * b2 + b1 * a2 + c1 * d2 - d1 * c2,
            a1 * c2 - b1 * d2 + c1 * a2 + d1 * b2,
            a1 * d2 + b1 * c2 - c1 * b2 + d1 * a2
        )

    abs q = error "abs not defined for Quat"
    signum q = error "signum not defined for Quat"

    fromInteger i = Quat (fromInteger i, 0, 0, 0)

quatRotation :: Num a => Quat a -> Vec3 (Vec3 a)
quatRotation (Quat (q0, q1, q2, q3)) = Vec3
    (Vec3 (q0 ^ 2 + q1 ^ 2 - q2 ^ 2 - q3 ^ 2) (2 * (q1 * q2 - q0 * q3)) (2 * (q1 * q3 + q0 * q2)))
    (Vec3 (2 * (q1 * q2 + q0 * q3)) (q0 ^ 2 - q1 ^ 2 + q2 ^ 2 - q3 ^ 2) (2 * (q2 * q3 - q0 * q1)))
    (Vec3 (2 * (q1 * q3 - q0 * q2)) (2 * (q2 * q3 + q0 * q1)) (q0 ^ 2 - q1 ^ 2 - q2 ^ 2 + q3 ^ 2))
