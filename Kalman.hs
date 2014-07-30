{-# LANGUAGE OverloadedStrings #-}
module Kalman where

import Matrix
import Quat
import SymDiff

import Data.List
import Data.String

newtype VarName = VarName String
    deriving (Eq, Ord)

instance Show VarName where
    show (VarName s) = s

instance IsString VarName where
    fromString = VarName . fromString

dt :: Sym VarName
dt = var "dt" -- IMU time step - sec

da, dv :: [VarName]
da = ["dax", "day", "daz"] -- IMU delta angle measurements in body axes - rad
dv = ["dvx", "dvy", "dvz"] -- IMU delta velocity measurements in body axes - m/sec

quatVars, vel, vel_R, pos, pos_R, da_b, vw, magNED, magXYZ :: [VarName]
quatVars = ["q0", "q1", "q2", "q3"] -- quaternions defining attitude of body axes relative to local NED
(vel, vel_R) = unzip [("vn", "R_VN"), ("ve", "R_VE"), ("vd", "R_VD")] -- NED velocity - m/sec
(pos, pos_R) = unzip [("pn", "R_PN"), ("pe", "R_PE"), ("pd", "R_PD")] -- NED position - m
da_b = ["dax_b", "day_b", "daz_b"] -- delta angle bias - rad
vw = ["vwn", "vwe"] -- NE wind velocity - m/sec
magNED = ["magN", "magE", "magD"] -- NED earth fixed magnetic field components - milligauss
magXYZ = ["magX", "magY", "magZ"] -- XYZ body fixed magnetic field measurements - milligauss

stateVector :: [VarName]
stateVector = quatVars ++ vel ++ pos ++ da_b ++ vw ++ magNED ++ magXYZ

nStates :: Int
nStates = length stateVector

quat :: Quat (Sym VarName)
quat = fmap var $ quatFromList quatVars

body2nav :: [[Sym VarName]]
body2nav = quatRotation quat

-- define the bias corrected delta angle
-- Ignore coning compensation and earths rotation as these effect are
-- negligible in terms of covariance growth compared to other efects for our
-- grade of sensor
-- deltaAngle = da - da_b + 1/12*cross(da_prev,da) - transpose(Cbn)*([omn; ome; omd])*dt;
deltaAngle :: [Sym VarName]
deltaAngle = zipWith (-) (map var da) (map var da_b)

-- define the attitude update equations
-- use a first order expansion of rotation to calculate the quaternion increment
-- acceptable for propagation of covariances
qNew :: [Sym VarName]
qNew = quatToList $ quatMult quat $ Quat
    ( 1
    -- XXX: why isn't dt in here somewhere?
    -- XXX: this can't generally produce a unit quaternion, can it?
    , 0.5 * (deltaAngle !! 0)
    , 0.5 * (deltaAngle !! 1)
    , 0.5 * (deltaAngle !! 2)
    )

-- XXX: because `g` is constant, it disappears from the Jacobian. so why is it here?
g :: [Sym VarName]
g = map var ["gn", "ge", "gd"] -- NED gravity vector - m/sec^2

-- define the velocity update equations
-- ignore coriolis terms for linearisation purposes
vNew :: [Sym VarName]
-- XXX: shouldn't dv be multiplied by dt too?
vNew = zipWith (+) (map var vel) $ zipWith (+) (map (* dt) g) $ matVecMult body2nav $ map var dv

pNew :: [Sym VarName]
pNew = zipWith (+) (map var pos) $ map ((* dt) . var) dv

processEqns :: [Sym VarName]
processEqns = qNew ++ vNew ++ pNew ++ map var (da_b ++ vw ++ magNED ++ magXYZ)

kalmanF :: [[Sym VarName]]
kalmanF = jacobian processEqns stateVector

-- Define the control (disturbance) vector. Error growth in the inertial
-- solution is assumed to be driven by 'noise' in the delta angles and
-- velocities, after bias effects have been removed. This is OK becasue we
-- have sensor bias accounted for in the state equations.
distVector :: [VarName]
distVector = da ++ dv

kalmanG :: [[Sym VarName]]
kalmanG = jacobian processEqns distVector

kalmanQ :: [[Sym VarName]]
kalmanQ = matMult kalmanG $ matMult imuNoise $ transpose kalmanG
    where
    imuNoise = diagMat $ map var ["daxCov", "dayCov", "dazCov", "dvxCov", "dvyCov", "dvzCov"]

kalmanP :: [[Sym VarName]]
kalmanP = [ [ var $ VarName $ "OP_" ++ show i ++ "_" ++ show j | j <- [1..nStates] ] | i <- [1..nStates] ]

kalmanPP :: [[Sym VarName]]
kalmanPP = matBinOp (+) kalmanQ $ matMult kalmanF $ matMult kalmanP $ transpose kalmanF

measurementUpdate :: [Sym VarName] -> [VarName] -> [[Sym VarName]] -> [[Sym VarName]] -> ([[Sym VarName]], [[Sym VarName]])
measurementUpdate measurements states errorCov obsCov = (obsModel, obsGain)
    where
    obsModel = jacobian measurements states
    ph = matMult errorCov $ transpose obsModel
    obsGain = matMult ph $ matInvert $ matBinOp (+) obsCov $ matMult obsModel ph

hk_vel :: [([[Sym VarName]], [[Sym VarName]])]
hk_vel = [ measurementUpdate [var v] stateVector kalmanP [[var r]] | (v, r) <- zip vel vel_R ]

hk_pos :: [([[Sym VarName]], [[Sym VarName]])]
hk_pos = [ measurementUpdate [var v] stateVector kalmanP [[var r]] | (v, r) <- zip pos pos_R ]

hk_tas :: ([[Sym VarName]], [[Sym VarName]])
hk_tas = measurementUpdate [((vn - vwn) ^. 2 + (ve - vwe) ^. 2 + vd ^. 2) ^. 0.5] stateVector kalmanP [[var "R_TAS"]]
    where
    [vn, ve, vd] = map var vel
    [vwn, vwe] = map var vw

hk_mag :: [([[Sym VarName]], [[Sym VarName]])]
hk_mag = [ measurementUpdate [v] stateVector kalmanP [[var "R_MAG"]] | v <- zipWith (+) (map var magXYZ) $ matVecMult (transpose body2nav) $ map var magNED ]
