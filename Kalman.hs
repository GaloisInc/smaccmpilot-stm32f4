{-# LANGUAGE OverloadedStrings #-}
module Kalman where

import Matrix
import Quat
import SymDiff

import Data.Foldable (Foldable(..), toList)
import Data.List
import Data.Monoid
import Data.String

-- For measurements/states in navigation frame
data NED a = NED { north :: a, east :: a, down :: a }
    deriving Show

instance Foldable NED where
    foldMap f ned = f (north ned) `mappend` f (east ned) `mappend` f (down ned)

instance Functor NED where
    fmap f ned = NED (f $ north ned) (f $ east ned) (f $ down ned)

instance Num a => Num (NED a) where
    (NED u1 u2 u3) + (NED v1 v2 v3) = NED (u1 + v1) (u2 + v2) (u3 + v3)
    (NED u1 u2 u3) * (NED v1 v2 v3) = NED (u2 * v3 - u3 * v2) (u3 * v1 - u1 * v3) (u1 * v2 - u2 * v1)
    negate = fmap negate
    fromInteger i = error "NED vectors can't be constructed by fromInteger"
    abs ned = error "NED vectors are not closed under abs"
    signum ned = error "NED vectors are not closed under signum"


-- For measurements/states in body frame
data XYZ a = XYZ { x :: a, y :: a, z :: a }
    deriving Show

instance Foldable XYZ where
    foldMap f xyz = f (x xyz) `mappend` f (y xyz) `mappend` f (z xyz)

instance Functor XYZ where
    fmap f xyz = XYZ (f $ x xyz) (f $ y xyz) (f $ z xyz)

instance Num a => Num (XYZ a) where
    (XYZ u1 u2 u3) + (XYZ v1 v2 v3) = XYZ (u1 + v1) (u2 + v2) (u3 + v3)
    (XYZ u1 u2 u3) * (XYZ v1 v2 v3) = XYZ (u2 * v3 - u3 * v2) (u3 * v1 - u1 * v3) (u1 * v2 - u2 * v1)
    negate = fmap negate
    fromInteger i = error "XYZ vectors can't be constructed by fromInteger"
    abs xyz = error "XYZ vectors are not closed under abs"
    signum xyz = error "XYZ vectors are not closed under signum"


-- Rotate between coordinate frames through a given quaternion
convertFrames :: Num a => Quat a -> (XYZ a -> NED a, NED a -> XYZ a)
convertFrames q = (toNav, toBody)
    where
    rotate2nav = quatRotation q
    convert mat mkVector = mkVector . matVecMult mat . toList
    toNav = convert rotate2nav (\[n, e, d]-> NED n e d)
    toBody = convert (transpose rotate2nav) (\[x, y, z]-> XYZ x y z)

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
    , stateVel = NED "vn" "ve" "vd" -- NED velocity - m/sec
    , statePos = NED "pn" "pe" "pd" -- NED position - m
    , stateGyroBias = XYZ "dax_b" "day_b" "daz_b" -- delta angle bias - rad
    , stateWind = NED "vwn" "vwe" "vwd" -- NE wind velocity - m/sec
    , stateMagNED = NED "magN" "magE" "magD" -- NED earth fixed magnetic field components - milligauss
    , stateMagXYZ = XYZ "magX" "magY" "magZ" -- XYZ body fixed magnetic field measurements - milligauss
    }

kalmanP :: IsString var => [[Sym var]]
kalmanP = [ [ var $ fromString $ "OP_" ++ show i ++ "_" ++ show j | j <- idxs ] | i <- idxs ]
    where
    idxs = [1..nStates]

distVector :: DisturbanceVector VarName
distVector = DisturbanceVector
    { disturbanceGyro = XYZ "dax" "day" "daz" -- IMU delta angle measurements in body axes - rad
    , disturbanceAccel = XYZ "dvx" "dvy" "dvz" -- IMU delta velocity measurements in body axes - m/sec
    }

distCovariance :: DisturbanceVector VarName
distCovariance = DisturbanceVector
    { disturbanceGyro = XYZ "daxCov" "dayCov" "dazCov"
    , disturbanceAccel = XYZ "dvxCov" "dvyCov" "dvzCov"
    }

velCovariance :: NED VarName
velCovariance = NED "R_VN" "R_VE" "R_VD"

posCovariance :: NED VarName
posCovariance = NED "R_PN" "R_PE" "R_PD"

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
    deltaAngle = disturbanceGyro dist - stateGyroBias state
    deltaQuat = Quat (0, x deltaAngle, y deltaAngle, z deltaAngle)
    -- XXX: shouldn't accel be multiplied by dt too?
    deltaVel = body2nav state (disturbanceAccel dist) + fmap (* dt) g
    g = NED 0 0 9.80665 -- NED gravity vector - m/sec^2

kalmanF :: Eq var => var -> StateVector var -> DisturbanceVector var -> [[Sym var]]
kalmanF dt state dist = jacobian (toList $ processModel (var dt) (fmap var state) (fmap var dist)) (toList state)

-- Define the control (disturbance) vector. Error growth in the inertial
-- solution is assumed to be driven by 'noise' in the delta angles and
-- velocities, after bias effects have been removed. This is OK becasue we
-- have sensor bias accounted for in the state equations.
kalmanG :: Eq var => var -> StateVector var -> DisturbanceVector var -> [[Sym var]]
kalmanG dt state dist = jacobian (toList $ processModel (var dt) (fmap var state) (fmap var dist)) (toList dist)

kalmanQ :: DisturbanceVector var -> [[Sym var]] -> [[Sym var]]
kalmanQ cov g = matMult g $ matMult (diagMat $ map var $ toList cov) $ transpose g

kalmanPP :: Eq var => var -> StateVector var -> DisturbanceVector var -> DisturbanceVector var -> [[Sym var]] -> [[Sym var]]
kalmanPP dt state dist cov p = matBinOp (+) q $ matMult f $ matMult p $ transpose f
    where
    f = kalmanF dt state dist
    g = kalmanG dt state dist
    q = kalmanQ cov g

type MeasurementModel var = ([Sym var], [(var, Sym var)], [[Sym var]])
measurementUpdate :: (Foldable t, Eq var) => t var -> [(var, Sym var)] -> [[Sym var]] -> [[Sym var]] -> MeasurementModel var
measurementUpdate state measurements obsCov errorCov = (innovation, state', errorCov')
    where
    innovation = [ var v - h | (v, h) <- measurements ]
    obsModel = jacobian (map snd measurements) (toList state)
    ph = matMult errorCov $ transpose obsModel
    obsGain = matMult ph $ matInvert $ matBinOp (+) obsCov $ matMult obsModel ph
    state' = [ (v, var v + update) | (v, update) <- zip (toList state) (matVecMult obsGain innovation) ]
    errorCov' = matBinOp (-) errorCov $ matMult (matMult obsGain obsModel) errorCov

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
