{-# OPTIONS_GHC -fno-warn-orphans #-}

module Numeric.Estimator.Model.SensorFusion where

import Control.Applicative
import Control.Lens
import Data.Distributive
import Data.Foldable
import Data.Traversable
import Linear
import Numeric.Estimator.Augment
import Numeric.Estimator.Model.Coordinate
import Numeric.Estimator.Model.Pressure
import Prelude hiding (foldl1, foldr, sum)

-- Linear's Num (Quaternion a) instance requires (RealFloat a) in order
-- to implement signum, but we don't want to require RealFloat as it
-- doesn't work for symbolic types. This is a copy of just the (*)
-- implementation, which only needed (Num a).
quatMul :: Num a => Quaternion a -> Quaternion a -> Quaternion a
quatMul (Quaternion s1 v1) (Quaternion s2 v2)
    = Quaternion (s1 * s2 - (v1 `dot` v2)) $ (v1 `cross` v2) + s1 *^ v2 + s2 *^ v1

data StateVector a = StateVector
    { stateOrient :: !(Quaternion a) -- quaternions defining attitude of body axes relative to local NED
    , stateVel :: !(NED a) -- NED velocity - m/sec
    , statePos :: !(NED a) -- NED position - m
    , stateGyroBias :: !(XYZ a) -- delta angle bias - rad
    , stateWind :: !(NED a) -- NED wind velocity - m/sec
    , stateMagNED :: !(NED a) -- NED earth fixed magnetic field components - milligauss
    , stateMagXYZ :: !(XYZ a) -- XYZ body fixed magnetic field measurements - milligauss
    }
    deriving Show

instance Additive StateVector where
    zero = pure 0

instance Applicative StateVector where
    pure v = StateVector
        { stateOrient = pure v
        , stateVel = pure v
        , statePos = pure v
        , stateGyroBias = pure v
        , stateWind = pure v
        , stateMagNED = pure v
        , stateMagXYZ = pure v
        }
    v1 <*> v2 = StateVector
        { stateOrient = stateOrient v1 <*> stateOrient v2
        , stateVel = stateVel v1 <*> stateVel v2
        , statePos = statePos v1 <*> statePos v2
        , stateGyroBias = stateGyroBias v1 <*> stateGyroBias v2
        , stateWind = stateWind v1 <*> stateWind v2
        , stateMagNED = stateMagNED v1 <*> stateMagNED v2
        , stateMagXYZ = stateMagXYZ v1 <*> stateMagXYZ v2
        }

instance Functor StateVector where
    fmap = liftA

instance Foldable StateVector where
    foldMap = foldMapDefault

instance Traversable StateVector where
    sequenceA v = StateVector
        <$> sequenceA (stateOrient v)
        <*> sequenceA (stateVel v)
        <*> sequenceA (statePos v)
        <*> sequenceA (stateGyroBias v)
        <*> sequenceA (stateWind v)
        <*> sequenceA (stateMagNED v)
        <*> sequenceA (stateMagXYZ v)

instance Distributive StateVector where
    distribute f = StateVector
        { stateOrient = distribute $ fmap stateOrient f
        , stateVel = distribute $ fmap stateVel f
        , statePos = distribute $ fmap statePos f
        , stateGyroBias = distribute $ fmap stateGyroBias f
        , stateWind = distribute $ fmap stateWind f
        , stateMagNED = distribute $ fmap stateMagNED f
        , stateMagXYZ = distribute $ fmap stateMagXYZ f
        }

-- Define the control (disturbance) vector. Error growth in the inertial
-- solution is assumed to be driven by 'noise' in the delta angles and
-- velocities, after bias effects have been removed. This is OK becasue we
-- have sensor bias accounted for in the state equations.
data DisturbanceVector a = DisturbanceVector
    { disturbanceGyro :: !(XYZ a) -- XYZ body rotation rate in rad/second
    , disturbanceAccel :: !(XYZ a) -- XYZ body acceleration in meters/second/second
    }
    deriving Show

instance Applicative DisturbanceVector where
    pure v = DisturbanceVector
        { disturbanceGyro = pure v
        , disturbanceAccel = pure v
        }
    v1 <*> v2 = DisturbanceVector
        { disturbanceGyro = disturbanceGyro v1 <*> disturbanceGyro v2
        , disturbanceAccel = disturbanceAccel v1 <*> disturbanceAccel v2
        }

instance Functor DisturbanceVector where
    fmap = liftA

instance Foldable DisturbanceVector where
    foldMap = foldMapDefault

instance Traversable DisturbanceVector where
    sequenceA v = DisturbanceVector
        <$> sequenceA (disturbanceGyro v)
        <*> sequenceA (disturbanceAccel v)

instance Distributive DisturbanceVector where
    distribute f = DisturbanceVector
        { disturbanceGyro = distribute $ fmap disturbanceGyro f
        , disturbanceAccel = distribute $ fmap disturbanceAccel f
        }

nStates :: Int
nStates = length $ toList (pure () :: StateVector ())

-- Model initialization

class HasAtan2 a where
    arctan2 :: a -> a -> a

instance HasAtan2 Double where
    arctan2 = atan2

kalmanP :: Fractional a => StateVector (StateVector a)
kalmanP = kronecker $ fmap (^ (2 :: Int)) $ StateVector
    { stateOrient = pure 0.1
    , stateVel = pure 0.7
    , statePos = ned 15 15 5
    , stateGyroBias = pure $ 1 * deg2rad
    , stateWind = pure 8
    , stateMagNED = pure 0.02
    , stateMagXYZ = pure 0.02
    }
    where
    deg2rad = realToFrac (pi :: Double) / 180

initAttitude :: (Floating a, HasAtan2 a) => XYZ a -> XYZ a -> a -> Quaternion a
initAttitude (XYZ accel) (XYZ mag) declination = foldl1 quatMul $ map (uncurry rotateAround)
    [ (ez, initialHdg)
    , (ey, initialPitch)
    , (ex, initialRoll)
    ]
    where
    initialRoll = arctan2 (negate (accel ^._y)) (negate (accel ^._z))
    initialPitch = arctan2 (accel ^._x) (negate (accel ^._z))
    magX = (mag ^._x) * cos initialPitch + (mag ^._y) * sin initialRoll * sin initialPitch + (mag ^._z) * cos initialRoll * sin initialPitch
    magY = (mag ^._y) * cos initialRoll - (mag ^._z) * sin initialRoll
    initialHdg = arctan2 (negate magY) magX + declination
    rotateAround axis theta = Quaternion (cos half) $ pure 0 & el axis .~ (sin half) where half = theta / 2

initDynamic :: (Floating a, HasAtan2 a) => XYZ a -> XYZ a -> XYZ a -> a -> NED a -> NED a -> StateVector a
initDynamic accel mag magBias declination vel pos = (pure 0)
    { stateOrient = initQuat
    , stateVel = vel
    , statePos = pos
    , stateMagNED = initMagNED
    , stateMagXYZ = magBias
    }
    where
    initMagXYZ = mag - magBias
    initQuat = initAttitude accel initMagXYZ declination
    initMagNED = fst (convertFrames initQuat) initMagXYZ
    -- TODO: re-implement InertialNav's calcEarthRateNED

-- Kalman equations

body2nav :: Num a => StateVector a -> XYZ a -> NED a
body2nav = fst . convertFrames . stateOrient
nav2body :: Num a => StateVector a -> NED a -> XYZ a
nav2body = snd . convertFrames . stateOrient

processModel :: Fractional a => a -> AugmentState StateVector DisturbanceVector a -> AugmentState StateVector DisturbanceVector a
processModel dt (AugmentState state dist) = AugmentState state' $ pure 0
    where
    state' = state
        -- Discretization of `qdot = 0.5 * <0, deltaAngle> * q`.
        -- * _Strapdown Inertial Navigation Technology, 2nd Ed_, section 11.2.5 (on
        --   pages 319-320) gives qdot and its analytic discretization, without proof.
        -- * http://en.wikipedia.org/wiki/Discretization derives the general form of
        --   discretization.
        -- * http://www.euclideanspace.com/physics/kinematics/angularvelocity/QuaternionDifferentiation2.pdf
        --   derives qdot from angular momentum.
        { stateOrient = stateOrient state `quatMul` deltaQuat
        , stateVel = stateVel state + deltaVel
        , statePos = statePos state + fmap (* dt) (stateVel state + fmap (/ 2) deltaVel)
        -- remaining state vector elements are unchanged by the process model
        }
    -- Even fairly low-order approximations introduce error small enough
    -- that it's swamped by other filter errors.
    deltaQuat = approxAxisAngle 3 $ xyzToVec3 $ fmap (* dt) $ disturbanceGyro dist - stateGyroBias state
    deltaVel = fmap (* dt) $ body2nav state (disturbanceAccel dist) + g
    g = ned 0 0 9.80665 -- NED gravity vector - m/sec^2

-- The Taylor series expansion of the quaternion axis-angle formula never
-- divides by any quantity that might be zero. It also avoids computing fancy
-- floating-point functions like sin, cos, or sqrt. And since the ARM chip
-- we're running on doesn't have those fancy functions in hardware, this
-- implementation is as efficient as we're going to get anyway.
approxAxisAngle :: Fractional a => Int -> V3 a -> Quaternion a
approxAxisAngle order rotation = Quaternion c $ fmap (s *) rotation
    where
    halfSigmaSq = 0.25 * sum (fmap (^ (2 :: Int)) rotation)
    go prev idx = let cosTerm = prev / fromIntegral (negate idx); sinTerm = cosTerm / fromIntegral (idx + 1) in cosTerm : sinTerm : go (sinTerm * halfSigmaSq) (idx + 2)
    combine term (l, r) = (r + term, l)
    (c, s2) = foldr combine (1, 1) $ take (order - 1) $ go halfSigmaSq (2 :: Int)
    s = 0.5 * s2

statePressure :: Floating a => StateVector a -> a
statePressure = heightToPressure . negate . (^._z) . nedToVec3 . statePos

stateTAS :: Floating a => StateVector a -> a
stateTAS state = distance (stateVel state) (stateWind state)

stateMag :: Num a => StateVector a -> XYZ a
stateMag state = stateMagXYZ state + nav2body state (stateMagNED state)
