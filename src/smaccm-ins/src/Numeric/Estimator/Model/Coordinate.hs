{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Description: Types for different coordinate systems

The 'Linear' module provides basic fixed-dimensional vector types such
as 'V3', for three-element vectors. However, it does not help with
identifying which coordinate system a vector was measured in.

The types in this module are trivial newtype wrappers around 'V3' to tag
vectors with an appropriate coordinate system. The systems used here
follow a common convention used in navigation problems.
-}

module Numeric.Estimator.Model.Coordinate where

import Control.Applicative
import Data.Distributive
import Data.Foldable
import Data.Traversable
import Linear

-- * Navigation frame

{- |
Navigation occurs in a right-hand coordinate system with respect to a
\"local tangent plane\". The origin of this plane is chosen to be some
convenient point on the Earth's surface--perhaps the location where
navigation began. The plane is oriented such that it is tangent to the
Earth's surface at that origin point. The basis vectors point northward,
eastward, and downward from the origin. Notice that the further you
travel from the origin, the further the tangent plane separates from the
surface of the Earth, so this approach is of limited use over long
distances.
-}
newtype NED a = NED { nedToVec3 :: V3 a }
    deriving (Show, Additive, Applicative, Distributive, Foldable, Functor, Metric, Num, Traversable)

-- | Construct a navigation frame coordinate from (north, east, down).
ned :: a -> a -> a -> NED a
ned n e d = NED $ V3 n e d

-- * Body frame

{- |
Most sensor measurements are taken with respect to the sensor
platform in the vehicle. We assume the sensors are perfectly
orthogonally arranged in a right-hand Cartesian coordinate system, which
is usually close enough to the truth, although more sophisticated
approaches exist to calibrate out non-orthogonal alignment and other
errors. This coordinate system is only meaningful with respect to the
current position and orientation of the sensor platform, as of the
instant that the measurement was taken.
-}
newtype XYZ a = XYZ { xyzToVec3 :: V3 a }
    deriving (Show, Additive, Applicative, Distributive, Foldable, Functor, Metric, Num, Traversable)

-- | Construct a body frame coordinate from (x, y, z).
xyz :: a -> a -> a -> XYZ a
xyz a b c = XYZ $ V3 a b c

-- * Coordinate frame conversion

{- |
Most practical problems involving inertial sensors (such as
accelerometers and gyroscopes) require keeping track of the relationship
between these two coordinate systems.

If you maintain a quaternion representing the rotation from navigation
frame to body frame, then you can use this function to get functions
that will convert coordinates between frames in either direction.
-}
convertFrames :: Num a => Quaternion a -> (XYZ a -> NED a, NED a -> XYZ a)
convertFrames q = (toNav, toBody)
    where
    rotate2nav = NED $ fmap XYZ $ fromQuaternion q
    toNav = (rotate2nav !*)
    toBody = (transpose rotate2nav !*)
