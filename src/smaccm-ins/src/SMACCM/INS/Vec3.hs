module SMACCM.INS.Vec3 where

import Control.Applicative
import Data.Distributive
import Data.Foldable
import Data.Traversable
import Prelude hiding (sum)
import SMACCM.INS.Matrix

data Vec3 a = Vec3 { vecX :: !a, vecY :: !a, vecZ :: !a }
    deriving (Eq, Show)

instance Applicative Vec3 where
    pure v = Vec3 v v v
    Vec3 x1 y1 z1 <*> Vec3 x2 y2 z2 = Vec3 (x1 x2) (y1 y2) (z1 z2)
instance Pointwise Vec3 where

instance Foldable Vec3 where
    foldMap = foldMapDefault

instance Functor Vec3 where
    fmap = liftA

instance Traversable Vec3 where
    sequenceA (Vec3 fx fy fz) = Vec3 <$> fx <*> fy <*> fz

instance Distributive Vec3 where
    distribute f = Vec3 (fmap vecX f) (fmap vecY f) (fmap vecZ f)

instance Num a => Num (Vec3 a) where
    v1 + v2 = (+) <$> v1 <*> v2
    (Vec3 u1 u2 u3) * (Vec3 v1 v2 v3) = Vec3 (u2 * v3 - u3 * v2) (u3 * v1 - u1 * v3) (u1 * v2 - u2 * v1)
    negate = fmap negate
    fromInteger _ = error "Vec3 vectors can't be constructed by fromInteger"
    abs _ = error "Vec3 vectors are not closed under abs"
    signum _ = error "Vec3 vectors are not closed under signum"

vecMag :: (Functor f, Foldable f, Floating a) => f a -> a
vecMag = sqrt . sum . fmap (^ (2 :: Int))