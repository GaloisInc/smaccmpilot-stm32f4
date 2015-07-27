-- | Model noise parameters

module SMACCMPilot.INS.Parameters where

import Control.Applicative
import Numeric.Estimator.Model.Coordinate
import SMACCMPilot.INS.SensorFusion

processNoise :: Fractional a => a -> StateVector a
processNoise dt = fmap (^ (2 :: Int)) $ fmap (dt *) $ StateVector
    { stateOrient = pure 1.0e-9
    , stateMagNED = pure 3.0e-0
    }

gyroNoise :: Fractional a => XYZ a
gyroNoise = pure (1.394590636121633e-3 ^ (2 :: Int))

magNoise :: Fractional a => XYZ a
magNoise = pure 1.4826

accelNoise :: Fractional a => XYZ a
accelNoise = pure (3.8031599738985476e-2 ^ (2 :: Int))
