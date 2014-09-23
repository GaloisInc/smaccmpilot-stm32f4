{-# LANGUAGE OverloadedStrings #-}
module Simulate where

import ExtendedKalmanFilter
import Kalman
import Quat
import SymDiff
import Vec3

import Data.String

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
