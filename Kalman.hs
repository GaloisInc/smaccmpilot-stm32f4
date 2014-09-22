{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Kalman where

import ExtendedKalmanFilter
import Matrix
import Quat
import SymDiff
import Vec3

import Control.Applicative
import Data.Foldable (Foldable(..), toList)
import Data.List
import Data.Monoid
import Data.String
import Data.Traversable

-- For measurements/states in navigation frame
newtype NED a = NED (Vec3 a)
    deriving (Show, Applicative, Foldable, Functor, Traversable, Num)

ned :: a -> a -> a -> NED a
ned n e d = NED $ Vec3 n e d

-- For measurements/states in body frame
newtype XYZ a = XYZ (Vec3 a)
    deriving (Show, Applicative, Foldable, Functor, Traversable, Num)

xyz :: a -> a -> a -> XYZ a
xyz a b c = XYZ $ Vec3 a b c

-- Rotate between coordinate frames through a given quaternion
convertFrames :: Num a => Quat a -> (XYZ a -> NED a, NED a -> XYZ a)
convertFrames q = (toNav, toBody)
    where
    rotate2nav = quatRotation q
    convert mat mkVector = mkVector . matVecMult mat . toList
    toNav = convert rotate2nav (\[n, e, d]-> ned n e d)
    toBody = convert (transpose rotate2nav) (\[x, y, z]-> xyz x y z)

data StateVector a = StateVector
    { stateOrient :: Quat a
    , stateVel :: NED a
    , statePos :: NED a
    , stateGyroBias :: XYZ a
    , stateWind :: NED a
    , stateMagNED :: NED a
    , stateMagXYZ :: XYZ a
    }
    deriving Show

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
    foldMap f v = mconcat
        [ foldMap f $ stateOrient v
        , foldMap f $ stateVel v
        , foldMap f $ statePos v
        , foldMap f $ stateGyroBias v
        , foldMap f $ stateWind v
        , foldMap f $ stateMagNED v
        , foldMap f $ stateMagXYZ v
        ]

instance Traversable StateVector where
    sequenceA v = StateVector
        <$> sequenceA (stateOrient v)
        <*> sequenceA (stateVel v)
        <*> sequenceA (statePos v)
        <*> sequenceA (stateGyroBias v)
        <*> sequenceA (stateWind v)
        <*> sequenceA (stateMagNED v)
        <*> sequenceA (stateMagXYZ v)

-- Define the control (disturbance) vector. Error growth in the inertial
-- solution is assumed to be driven by 'noise' in the delta angles and
-- velocities, after bias effects have been removed. This is OK becasue we
-- have sensor bias accounted for in the state equations.
data DisturbanceVector a = DisturbanceVector
    { disturbanceGyro :: XYZ a
    , disturbanceAccel :: XYZ a
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
    foldMap f v = mconcat
        [ foldMap f $ disturbanceGyro v
        , foldMap f $ disturbanceAccel v
        ]

instance Traversable DisturbanceVector where
    sequenceA v = DisturbanceVector
        <$> sequenceA (disturbanceGyro v)
        <*> sequenceA (disturbanceAccel v)

nStates :: Int
nStates = length $ toList (pure () :: StateVector ())

-- Sample inputs

newtype VarName = VarName String
    deriving (Eq, Ord)

instance Show VarName where
    show (VarName s) = s

instance IsString VarName where
    fromString = VarName . fromString

stateVector :: StateVector VarName
stateVector = StateVector
    { stateOrient = Quat ("q0", "q1", "q2", "q3") -- quaternions defining attitude of body axes relative to local NED
    , stateVel = ned "vn" "ve" "vd" -- NED velocity - m/sec
    , statePos = ned "pn" "pe" "pd" -- NED position - m
    , stateGyroBias = xyz "dax_b" "day_b" "daz_b" -- delta angle bias - rad
    , stateWind = ned "vwn" "vwe" "vwd" -- NE wind velocity - m/sec
    , stateMagNED = ned "magN" "magE" "magD" -- NED earth fixed magnetic field components - milligauss
    , stateMagXYZ = xyz "magX" "magY" "magZ" -- XYZ body fixed magnetic field measurements - milligauss
    }

kalmanP :: IsString var => [[Sym var]]
kalmanP = [ [ var $ fromString $ "OP_" ++ show i ++ "_" ++ show j | j <- idxs ] | i <- idxs ]
    where
    idxs = [1..nStates]

distVector :: DisturbanceVector VarName
distVector = DisturbanceVector
    { disturbanceGyro = xyz "dax" "day" "daz" -- IMU delta angle measurements in body axes - rad
    , disturbanceAccel = xyz "dvx" "dvy" "dvz" -- IMU delta velocity measurements in body axes - m/sec
    }

distCovariance :: DisturbanceVector VarName
distCovariance = DisturbanceVector
    { disturbanceGyro = xyz "daxCov" "dayCov" "dazCov"
    , disturbanceAccel = xyz "dvxCov" "dvyCov" "dvzCov"
    }

velCovariance :: NED VarName
velCovariance = ned "R_VN" "R_VE" "R_VD"

posCovariance :: NED VarName
posCovariance = ned "R_PN" "R_PE" "R_PD"

tasCovariance :: VarName
tasCovariance = "R_TAS"

magCovariance :: VarName
magCovariance = "R_MAG"

-- Kalman equations

body2nav :: Num a => StateVector a -> XYZ a -> NED a
body2nav = fst . convertFrames . stateOrient
nav2body :: Num a => StateVector a -> NED a -> XYZ a
nav2body = snd . convertFrames . stateOrient

processModel :: (Num a, Fractional a) => a -> StateVector a -> DisturbanceVector a -> StateVector a
processModel dt state dist = state
    -- This approximates the discretization of `qdot = 0.5 * <0, deltaAngle> * q`.
    -- It assumes that dt is sufficiently small. The closed-form analytic
    -- discretization requires dividing by |deltaAngle|, which may be 0.
    -- * _Strapdown Inertial Navigation Technology, 2nd Ed_, section 11.2.5 (on
    --   pages 319-320) gives qdot and its analytic discretization, without proof.
    -- * http://en.wikipedia.org/wiki/Discretization derives the general form of
    --   discretization, and mentions this approximation.
    -- * http://www.euclideanspace.com/physics/kinematics/angularvelocity/QuaternionDifferentiation2.pdf
    --   derives qdot from angular momentum.
    { stateOrient = (1 + fmap (* (dt / 2)) deltaQuat) * stateOrient state
    , stateVel = stateVel state + deltaVel
    , statePos = statePos state + fmap (* dt) (stateVel state + fmap (/ 2) deltaVel)
    -- remaining state vector elements are unchanged by the process model
    }
    where
    (XYZ (Vec3 deltaX deltaY deltaZ)) = disturbanceGyro dist - stateGyroBias state
    deltaQuat = Quat (0, deltaX, deltaY, deltaZ)
    -- XXX: shouldn't accel be multiplied by dt too?
    deltaVel = body2nav state (disturbanceAccel dist) + fmap (* dt) g
    g = ned 0 0 9.80665 -- NED gravity vector - m/sec^2

-- A Fusion is a function from measurement covariance and measurement to
-- innovation, new state, and new estimated state covariance. This version only
-- supports scalar measurements. It's useful for sequential fusion. It's also
-- useful for partial measurements, such as measuring only altitude when you've
-- modeled 3D position.
type Fusion var = var -> var -> (Sym var, StateVector (Sym var), [[Sym var]])
fusion :: Eq var => StateVector var -> [[Sym var]] -> Sym var -> Fusion var
fusion state p v cov m = let ([innov], state', p') = measurementUpdate state [(m, v)] [[var cov]] p in (innov, state', p')

fuseVel :: Eq var => StateVector var -> [[Sym var]] -> NED (Fusion var)
fuseVel state p = fusion state p <$> fmap var (stateVel state)

fusePos :: Eq var => StateVector var -> [[Sym var]] -> NED (Fusion var)
fusePos state p = fusion state p <$> fmap var (statePos state)

fuseTAS :: Eq var => StateVector var -> [[Sym var]] -> Fusion var
fuseTAS state p = fusion state p (sqrt $ sum $ map (** 2) $ toList $ stateVel stateSym - stateWind stateSym)
    where
    stateSym = fmap var state

fuseMag :: Eq var => StateVector var -> [[Sym var]] -> XYZ (Fusion var)
fuseMag state p = fusion state p <$> stateMagXYZ stateSym + nav2body stateSym (stateMagNED stateSym)
    where
    stateSym = fmap var state
