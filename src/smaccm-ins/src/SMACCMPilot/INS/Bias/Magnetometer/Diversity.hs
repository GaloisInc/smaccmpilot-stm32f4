
module SMACCMPilot.INS.Bias.Magnetometer.Diversity
  ( monitorDiverseMagBiasEstimator
  , ivoryDiverseMagBiasEstimator
  , MagBiasEstimator(..)
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib
import SMACCMPilot.INS.Bias.Magnetometer


monitorDiverseMagBiasEstimator :: Monitor e MagBiasEstimator
monitorDiverseMagBiasEstimator = do
  n <- freshname "magbias"
  let (f, moduledef) = ivoryDiverseMagBiasEstimator (showUnique n)
  monitorModuleDef moduledef
  return f

ivoryDiverseMagBiasEstimator :: String -> (MagBiasEstimator, ModuleDef)
ivoryDiverseMagBiasEstimator n = (f, moddef)
  where
  (mbe, mbe_moddef) = ivoryMagBiasEstimator n
  f = MagBiasEstimator
    { mbe_init = do
        mbe_init mbe
    , mbe_sample = \s -> do
        -- TODO: only use sample if sufficiently far from existing samples
        mbe_sample mbe s
    , mbe_output = \o -> mbe_output mbe o
    }
  moddef = do
    -- TODO: create buffer of existing samples, function to insert into
    -- buffer, and function that gives distance of current sample from nearest
    -- sample in buffer
    mbe_moddef
  named nn = n ++ "_diversity_" ++ nn

