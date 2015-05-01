{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Control.Altitude.Filter where

import Ivory.Language
import Ivory.Stdlib

import SMACCMPilot.Flight.Types.MaybeFloat ()

-- | Return the change in a value, filtered by a low pass filter.
--
-- 'freq' and 'dt' must be known at Haskell run-time.
lowPassDeriv :: Float                         -- freq
             -> Float                         -- dt
             -> IFloat                        -- new value
             -> Ref s (Struct "maybe_float")  -- prev value
             -> Ivory eff IFloat              -- filtered delta
lowPassDeriv freq dt x prev = do
  let rc     = 1.0 / (2.0 * pi * freq)
  let alpha  = ifloat (dt / (rc + dt))
  base      <- getMaybe (constRef prev) x
  dx        <- assign $ alpha * (x - base)
  setJust prev (base + dx)
  return dx

-- | Return a changing value filtered by a low pass filter.
--
-- 'freq' and 'dt' must be known at Haskell run-time.
lowPassFilter :: Float                        -- freq
              -> Float                        -- dt
              -> IFloat                       -- new value
              -> Ref s (Struct "maybe_float") -- prev value
              -> Ivory eff IFloat             -- filtered value
lowPassFilter freq dt x prev = do
  _ <- lowPassDeriv freq dt x prev
  getMaybe (constRef prev) x

-- | Low pass filter with a dynamic timestep.
lowPassDeriv' :: Float
               -> IFloat
               -> IFloat
               -> Ref s (Struct "maybe_float")
               -> Ivory eff IFloat
lowPassDeriv' freq dt x prev = do
  rc        <- assign (1.0 / (2.0 * pi * ifloat freq))
  alpha     <- assign (dt / (rc + dt))
  base      <- getMaybe (constRef prev) x
  dx        <- assign $ alpha * (x - base)
  setJust prev (base + dx)
  return (x - base)

-- | Low pass filter with a dynamic timestep.
lowPassFilter' :: Float
               -> IFloat
               -> IFloat
               -> Ref s (Struct "maybe_float")
               -> Ivory eff IFloat
lowPassFilter' freq dt x prev = do
  rc        <- assign (1.0 / (2.0 * pi * ifloat freq))
  alpha     <- assign (dt / (rc + dt))
  base      <- getMaybe (constRef prev) x
  dx        <- assign $ alpha * (x - base)
  setJust prev (base + dx)
  return (base + dx)

