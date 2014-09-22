{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Kalman where

import ExtendedKalmanFilter
import Matrix
import Quat
import SymDiff
import Vec3

import Data.Foldable (Foldable(..), toList)
import Data.List
import Data.Monoid
import Data.String

-- For measurements/states in navigation frame
newtype NED a = NED (Vec3 a)
    deriving (Show, Foldable, Functor, Num)

ned :: a -> a -> a -> NED a
ned n e d = NED $ Vec3 n e d

-- For measurements/states in body frame
newtype XYZ a = XYZ (Vec3 a)
    deriving (Show, Foldable, Functor, Num)

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

instance Functor StateVector where
    fmap f v = StateVector
        { stateOrient = fmap f $ stateOrient v
        , stateVel = fmap f $ stateVel v
        , statePos = fmap f $ statePos v
        , stateGyroBias = fmap f $ stateGyroBias v
        , stateWind = fmap f $ stateWind v
        , stateMagNED = fmap f $ stateMagNED v
        , stateMagXYZ = fmap f $ stateMagXYZ v
        }

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

-- Define the control (disturbance) vector. Error growth in the inertial
-- solution is assumed to be driven by 'noise' in the delta angles and
-- velocities, after bias effects have been removed. This is OK becasue we
-- have sensor bias accounted for in the state equations.
data DisturbanceVector a = DisturbanceVector
    { disturbanceGyro :: XYZ a
    , disturbanceAccel :: XYZ a
    }
    deriving Show

instance Functor DisturbanceVector where
    fmap f v = DisturbanceVector
        { disturbanceGyro = fmap f $ disturbanceGyro v
        , disturbanceAccel = fmap f $ disturbanceAccel v
        }

instance Foldable DisturbanceVector where
    foldMap f v = mconcat
        [ foldMap f $ disturbanceGyro v
        , foldMap f $ disturbanceAccel v
        ]

nStates :: Int
nStates = length $ toList stateVector

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

fuseVel :: Eq var => NED var -> NED var -> StateVector var -> [[Sym var]] -> [MeasurementModel var]
fuseVel cov meas state p = [ measurementUpdate state [(m, var v)] [[var r]] p | (v, r, m) <- zip3 (toList $ stateVel state) (toList cov) (toList meas) ]

fusePos :: Eq var => NED var -> NED var -> StateVector var -> [[Sym var]] -> [MeasurementModel var]
fusePos cov meas state p = [ measurementUpdate state [(m, var v)] [[var r]] p | (v, r, m) <- zip3 (toList $ statePos state) (toList cov) (toList meas) ]

fuseTAS :: Eq var => var -> var -> StateVector var -> [[Sym var]] -> MeasurementModel var
fuseTAS cov meas state p = measurementUpdate state [(meas, sqrt $ sum $ map (** 2) $ toList $ stateVel stateSym - stateWind stateSym)] [[var cov]] p
    where
    stateSym = fmap var state

fuseMag :: Eq var => var -> XYZ var -> StateVector var -> [[Sym var]] -> [MeasurementModel var]
fuseMag cov meas state p = [ measurementUpdate state [(m, v)] [[var cov]] p | (v, m) <- zip (toList $ stateMagXYZ stateSym + nav2body stateSym (stateMagNED stateSym)) (toList meas) ]
    where
    stateSym = fmap var state
