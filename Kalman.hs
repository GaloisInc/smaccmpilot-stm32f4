{-# LANGUAGE OverloadedStrings #-}
module Kalman where

import Matrix
import Quat
import SymDiff

import Data.Foldable
import Data.List
import Data.Monoid
import Data.String

newtype VarName = VarName String
    deriving (Eq, Ord)

instance Show VarName where
    show (VarName s) = s

instance IsString VarName where
    fromString = VarName . fromString


-- For measurements/states in navigation frame
data NED a = NED { north :: a, east :: a, down :: a }

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


dt :: Sym VarName
dt = var "dt" -- IMU time step - sec

da, dv :: XYZ VarName
da = XYZ "dax" "day" "daz" -- IMU delta angle measurements in body axes - rad
dv = XYZ "dvx" "dvy" "dvz" -- IMU delta velocity measurements in body axes - m/sec

quatVars :: Quat VarName
quatVars = Quat ("q0", "q1", "q2", "q3") -- quaternions defining attitude of body axes relative to local NED

vel, vel_R, pos, pos_R :: NED VarName
vel = NED "vn" "ve" "vd" -- NED velocity - m/sec
pos = NED "pn" "pe" "pd" -- NED position - m
vel_R = NED "R_VN" "R_VE" "R_VD"
pos_R = NED "R_PN" "R_PE" "R_PD"
da_b :: XYZ VarName
da_b = XYZ "dax_b" "day_b" "daz_b" -- delta angle bias - rad
vw :: [VarName]
vw = ["vwn", "vwe"] -- NE wind velocity - m/sec
magNED :: NED VarName
magNED = NED "magN" "magE" "magD" -- NED earth fixed magnetic field components - milligauss
magXYZ :: XYZ VarName
magXYZ = XYZ "magX" "magY" "magZ" -- XYZ body fixed magnetic field measurements - milligauss

stateVector :: [VarName]
stateVector = toList quatVars ++ toList vel ++ toList pos ++ toList da_b ++ vw ++ toList magNED ++ toList magXYZ

nStates :: Int
nStates = length stateVector

quat :: Quat (Sym VarName)
quat = fmap var quatVars

body2nav :: XYZ (Sym VarName) -> NED (Sym VarName)
nav2body :: NED (Sym VarName) -> XYZ (Sym VarName)
(body2nav, nav2body) = convertFrames quat

-- define the bias corrected delta angle
-- Ignore coning compensation and earths rotation as these effect are
-- negligible in terms of covariance growth compared to other efects for our
-- grade of sensor
-- deltaAngle = da - da_b + 1/12*cross(da_prev,da) - transpose(Cbn)*([omn; ome; omd])*dt;
deltaAngle :: XYZ (Sym VarName)
deltaAngle = fmap var da - fmap var da_b

-- define the attitude update equations
-- use a first order expansion of rotation to calculate the quaternion increment
-- acceptable for propagation of covariances
qNew :: Quat (Sym VarName)
qNew = quat * Quat
    ( cos (rotationMag / 2)
    -- XXX: why isn't dt in here somewhere?
    , x deltaAngle * rotScalar
    , y deltaAngle * rotScalar
    , z deltaAngle * rotScalar
    )
    where
    rotationMag = sqrt (x deltaAngle ** 2 + y deltaAngle ** 2 + z deltaAngle ** 2)
    rotScalar = sin (rotationMag / 2) / rotationMag

-- XXX: because `g` is constant, it disappears from the Jacobian. so why is it here?
g :: NED (Sym VarName)
g = NED 0 0 9.80665 -- NED gravity vector - m/sec^2

-- define the velocity update equations
-- ignore coriolis terms for linearisation purposes
vNew :: NED (Sym VarName)
-- XXX: shouldn't dv be multiplied by dt too?
vNew = fmap var vel + body2nav (fmap var dv) + fmap (* dt) g

pNew :: NED (Sym VarName)
pNew = fmap var pos + fmap (* dt) (fmap var vel + fmap (/ 2) (body2nav (fmap var dv)))

processEqns :: [Sym VarName]
processEqns = toList qNew ++ toList vNew ++ toList pNew ++ map var (toList da_b ++ vw ++ toList magNED ++ toList magXYZ)

kalmanF :: [[Sym VarName]]
kalmanF = jacobian processEqns stateVector

-- Define the control (disturbance) vector. Error growth in the inertial
-- solution is assumed to be driven by 'noise' in the delta angles and
-- velocities, after bias effects have been removed. This is OK becasue we
-- have sensor bias accounted for in the state equations.
distVector :: [VarName]
distVector = toList da ++ toList dv

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
hk_vel = [ measurementUpdate [var v] stateVector kalmanP [[var r]] | (v, r) <- zip (toList vel) (toList vel_R) ]

hk_pos :: [([[Sym VarName]], [[Sym VarName]])]
hk_pos = [ measurementUpdate [var v] stateVector kalmanP [[var r]] | (v, r) <- zip (toList pos) (toList pos_R) ]

hk_tas :: ([[Sym VarName]], [[Sym VarName]])
hk_tas = measurementUpdate [sqrt ((var (north vel) - vwn) ** 2 + (var (east vel) - vwe) ** 2 + var (down vel) ** 2)] stateVector kalmanP [[var "R_TAS"]]
    where
    [vwn, vwe] = map var vw

hk_mag :: [([[Sym VarName]], [[Sym VarName]])]
hk_mag = [ measurementUpdate [v] stateVector kalmanP [[var "R_MAG"]] | v <- toList $ fmap var magXYZ + nav2body (fmap var magNED) ]
