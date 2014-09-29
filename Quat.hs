module Quat where

import Control.Applicative
import Data.Distributive
import Data.Foldable
import Data.Traversable
import Matrix (dotp)
import Vec3

data Quat a = Quat { quatScalar :: !a, quatVector :: !(Vec3 a) }
    deriving (Eq, Show)

instance Applicative Quat where
    pure v = Quat v $ pure v
    (Quat s1 v1) <*> (Quat s2 v2) = Quat (s1 s2) (v1 <*> v2)

instance Functor Quat where
    fmap = liftA

instance Foldable Quat where
    foldMap = foldMapDefault

instance Traversable Quat where
    sequenceA (Quat fs fv) = Quat <$> fs <*> sequenceA fv

instance Distributive Quat where
    distribute f = Quat (fmap quatScalar f) (distribute $ fmap quatVector f)

instance Num a => Num (Quat a) where
    q1 + q2 = (+) <$> q1 <*> q2
    negate = fmap negate
    (Quat s1 v1) * (Quat s2 v2) = Quat (s1 * s2 - dotp v1 v2) (fmap (s1 *) v2 + fmap (s2 *) v1 + v1 * v2)
    fromInteger i = Quat (fromInteger i) (pure 0)

    abs _ = error "abs not defined for Quat"
    signum _ = error "signum not defined for Quat"

fromAxisAngle :: Floating a => Vec3 a -> a -> Quat a
fromAxisAngle axis angle = Quat (cos (angle / 2)) (fmap (sin (angle / 2) *) axis)

quatRotation :: Num a => Quat a -> Vec3 (Vec3 a)
quatRotation (Quat q0 (Vec3 q1 q2 q3)) = Vec3
    (Vec3 (sq q0 + sq q1 - sq q2 - sq q3) (2 * (q1 * q2 - q0 * q3)) (2 * (q1 * q3 + q0 * q2)))
    (Vec3 (2 * (q1 * q2 + q0 * q3)) (sq q0 - sq q1 + sq q2 - sq q3) (2 * (q2 * q3 - q0 * q1)))
    (Vec3 (2 * (q1 * q3 - q0 * q2)) (2 * (q2 * q3 + q0 * q1)) (sq q0 - sq q1 - sq q2 + sq q3))
    where
    sq x = x ^ (2 :: Int)
