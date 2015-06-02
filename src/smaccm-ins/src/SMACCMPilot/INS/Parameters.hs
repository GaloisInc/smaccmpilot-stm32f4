-- | Model noise parameters

module SMACCMPilot.INS.Parameters where

import Control.Applicative
import Numeric.Estimator.Model.Coordinate
import SMACCMPilot.INS.SensorFusion

processNoise :: Fractional a => a -> StateVector a
processNoise dt = fmap (^ (2 :: Int)) $ fmap (dt *) $ StateVector
    { stateOrient = pure 1.0e-9
    , stateVel = pure 1.0e-9
    , statePos = pure 1.0e-9
    , stateGyroBias = pure 5.0e-7
    , stateWind = pure 0.1
    , stateMagNED = pure 3.0e-4
    , stateMagXYZ = pure 3.0e-4
    }

distCovariance :: Fractional a => DisturbanceVector a
distCovariance = fmap (^ (2 :: Int)) $ DisturbanceVector
    { disturbanceGyro = pure 7.762875447020379e-3
    , disturbanceAccel = pure 0.05
    }

velNoise :: Fractional a => NED a
velNoise = ned 0.04 0.04 0.08

posNoise :: Fractional a => NED a
posNoise = pure 4

pressureNoise :: Fractional a => a
pressureNoise = 128.39316

tasNoise :: Fractional a => a
tasNoise = 2

magNoise :: Fractional a => XYZ a
magNoise = pure 1.4826
