module Vec3 where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable

data Vec3 a = Vec3 a a a
    deriving Show

instance Foldable Vec3 where
    foldMap f (Vec3 x y z) = f x `mappend` f y `mappend` f z

instance Functor Vec3 where
    fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance Traversable Vec3 where
    sequenceA (Vec3 fx fy fz) = Vec3 <$> fx <*> fy <*> fz

instance Num a => Num (Vec3 a) where
    (Vec3 u1 u2 u3) + (Vec3 v1 v2 v3) = Vec3 (u1 + v1) (u2 + v2) (u3 + v3)
    (Vec3 u1 u2 u3) * (Vec3 v1 v2 v3) = Vec3 (u2 * v3 - u3 * v2) (u3 * v1 - u1 * v3) (u1 * v2 - u2 * v1)
    negate = fmap negate
    fromInteger i = error "Vec3 vectors can't be constructed by fromInteger"
    abs vec = error "Vec3 vectors are not closed under abs"
    signum vec = error "Vec3 vectors are not closed under signum"
