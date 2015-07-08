{- |
Description: Sample estimator model for sensor fusion

Many kinds of vehicles have a collection of sensors for measuring where
they are and where they're going, which may include these sensors and
others:

- accelerometers

- gyroscopes

- GPS receiver

- pressure altimeter

- 3D magnetometer

Each of these sensors provides some useful information about the current
physical state of the vehicle, but they all have two obnoxious problems:

1. No one sensor provides all the information you want at the update
rate you need. GPS gives you absolute position, but at best only ten
times per second. Accelerometers can report measurements at high speeds,
hundreds to thousands of times per second, but to get position you have
to double-integrate the measurement samples.

2. Every sensor is lying to you. They measure some aspect of the
physical state, plus some random error. If you have to integrate these
measurements, as with acceleration for instance, then the error
accumulates over time. If you take the derivative, perhaps because you
have position but you need velocity, the derivative amplifies the noise.

This is an ideal case for a state-space estimation algorithm. Once
you've specified the kinetic model of the physical system, and modeled
each of your sensors, and identified the noise parameters for
everything, the estimation algorithm is responsible for combining all
the measurements. The estimator will decide how much to trust each
sensor based on how much confidence it has in its current state
estimate, and how well that state agrees with the current measurement.

This module implements a system model for sensor fusion. With
appropriate noise parameters, it should work for a wide variety of
vehicle types and sensor platforms, whether on land, sea, air, or space.
However, it has been implemented specifically for quad-copter
autopilots. As a result the state vector may have components your system
does not need, or be missing ones you do need.
-}

module SMACCMPilot.INS.SensorFusion where

import Control.Applicative
import Control.Lens
import Data.Distributive
import Data.Foldable
import Data.Traversable
import Linear
import Numeric.Estimator.Augment
import Numeric.Estimator.Model.Coordinate
import Numeric.Estimator.Model.Symbolic
import SMACCMPilot.INS.Quaternion

-- | A collection of all the state variables needed for this model.
data StateVector a = StateVector
    { stateOrient :: !(Quaternion a) -- ^ quaternions defining attitude of body axes relative to local NED
    , stateMagNED :: !(NED a) -- ^ NED earth fixed magnetic field components - milligauss
    }
    deriving Show

instance Additive StateVector where
    zero = pure 0

instance Applicative StateVector where
    pure v = StateVector
        { stateOrient = pure v
        , stateMagNED = pure v
        }
    v1 <*> v2 = StateVector
        { stateOrient = stateOrient v1 <*> stateOrient v2
        , stateMagNED = stateMagNED v1 <*> stateMagNED v2
        }

instance Functor StateVector where
    fmap = liftA

instance Foldable StateVector where
    foldMap = foldMapDefault

instance Traversable StateVector where
    sequenceA v = StateVector
        <$> sequenceA (stateOrient v)
        <*> sequenceA (stateMagNED v)

instance Distributive StateVector where
    distribute f = StateVector
        { stateOrient = distribute $ fmap stateOrient f
        , stateMagNED = distribute $ fmap stateMagNED f
        }

-- * Model initialization

-- | Initial covariance for this model.
initCovariance :: Fractional a => StateVector (StateVector a)
initCovariance = scaled $ fmap (^ (2 :: Int)) $ StateVector
    { stateOrient = pure 0.1
    , stateMagNED = pure 0.02
    }

-- | When the sensor platform is not moving, a three-axis accelerometer
-- will sense an approximately 1g acceleration in the direction of
-- gravity, which gives us the platform's orientation aside from not
-- knowing the current rotation around the gravity vector.
--
-- At the same time, a 3D magnetometer will sense the platform's
-- orientation with respect to the local magnetic field, aside from not
-- knowing the current rotation around the magnetic field line.
--
-- Putting these two together gives the platform's complete orientation
-- since the gravity vector and magnetic field line aren't collinear.
--
-- http://cache.freescale.com/files/sensors/doc/app_note/AN4248.pdf
initAttitude :: (Floating a, HasAtan2 a)
             => XYZ a
             -- ^ initial accelerometer reading
             -> XYZ a
             -- ^ initial magnetometer reading
             -> a
             -- ^ local magnetic declination from true North
             -> Quaternion a
             -- ^ computed initial attitude
initAttitude (XYZ accel) (XYZ mag) declination = fromEuler initialRoll initialPitch initialHdg
    where
    initialRoll = arctan2 (accel ^._y) (accel ^._z)
    initialPitch = atan (negate (accel ^._x) / (accel ^._y * sin initialRoll + accel ^._z * cos initialRoll))
    magX = (mag ^._x) * cos initialPitch + (mag ^._y) * sin initialPitch * sin initialRoll + (mag ^._z) * sin initialPitch * cos initialRoll
    magY = (mag ^._z) * sin initialRoll - (mag ^._y) * cos initialRoll
    initialHdg = arctan2 magY magX + declination

-- | Compute an initial filter state from an assortment of initial
-- measurements.
initDynamic :: (Floating a, HasAtan2 a)
            => XYZ a
             -- ^ initial accelerometer reading
            -> XYZ a
             -- ^ initial magnetometer reading
            -> a
             -- ^ local magnetic declination from true North
            -> StateVector a
             -- ^ computed initial state
initDynamic accel mag declination = StateVector
    { stateOrient = initQuat
    , stateMagNED = initMagNED
    }
    where
    initQuat = initAttitude accel mag declination
    initMagNED = fst (convertFrames initQuat) mag
    -- TODO: re-implement InertialNav's calcEarthRateNED

-- * Model equations

-- | This is the kinematic sensor fusion process model, driven by
-- accelerometer and gyro measurements.
processModel :: Fractional a
             => a
             -- ^ time since last process model update
             -> AugmentState StateVector XYZ a
             -- ^ prior (augmented) state
             -> AugmentState StateVector XYZ a
             -- ^ posterior (augmented) state
processModel dt (AugmentState state gyro) = AugmentState state' $ pure 0
    where
    state' = state
        -- Discretization of @qdot = 0.5 * <0, deltaAngle> * q@.
        --
        --  * /Strapdown Inertial Navigation Technology, 2nd Ed/, section 11.2.5 (on
        --    pages 319-320) gives qdot and its analytic discretization, without proof.
        --  * http://en.wikipedia.org/wiki/Discretization derives the general form of
        --    discretization.
        --  * http://www.euclideanspace.com/physics/kinematics/angularvelocity/QuaternionDifferentiation2.pdf
        --    derives qdot from angular momentum.
        { stateOrient = stateOrient state `quatMul` deltaQuat
        -- remaining state vector elements are unchanged by the process model
        }
    -- Even fairly low-order approximations introduce error small enough
    -- that it's swamped by other filter errors.
    deltaQuat = approxAxisAngle 3 $ xyzToVec3 $ fmap (* dt) gyro

-- | Compute the expected body-frame magnetic field strength and
-- direction, given the hard-iron correction and local
-- declination-adjusted field from the state vector. Useful as a
-- measurement model for a 3D magnetometer.
stateMag :: Num a => StateVector a -> XYZ a
stateMag state = nav2body state (stateMagNED state)

stateAccel :: Fractional a => StateVector a -> XYZ a
stateAccel state = nav2body state (ned 0 0 9.80665)

-- * Helpers

-- | Convert body-frame to navigation-frame given the orientation from
-- this state vector.
body2nav :: Num a => StateVector a -> XYZ a -> NED a
body2nav = fst . convertFrames . stateOrient

-- | Convert navigation-frame to body-frame given the orientation from
-- this state vector.
nav2body :: Num a => StateVector a -> NED a -> XYZ a
nav2body = snd . convertFrames . stateOrient
