{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
--
-- Flight/Param.hs --- SMACCMPilot flight parameters.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module SMACCMPilot.Flight.Param where

import Control.Applicative ((<*>), (<$>))
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import SMACCMPilot.Param (Param, ParamT, param, group)

-- | PID controller parameters.
data PIDParams f = PIDParams
  { pidP    :: Param f
  , pidI    :: Param f
  , pidD    :: Param f
  , pidImax :: Param f
  } deriving (Functor, Foldable, Traversable)

-- | Initialize PID parameters with default values.
pidParams :: Monad m => Float -> Float -> Float -> Float
          -> ParamT f m (PIDParams f)
pidParams p i d imax =
  PIDParams <$> param "P" p
            <*> param "I" i
            <*> param "D" d
            <*> param "IMAX" imax

-- | Altitude hold controller parameters.
data AltHoldParams f = AltHoldParams
  { altHoldTrimThrottle  :: Param f
  , altHoldThrottleRate  :: PIDParams f
  , altHoldThrottleAccel :: PIDParams f
  , altHoldThrottleAlt   :: PIDParams f
  } deriving (Functor, Foldable, Traversable)

-- | Initialize alt hold parameters to default values.
--
-- XXX check that these IMAX values make sense, copied from ArduCopter
-- and divided by 100
altHoldParams :: Monad m => ParamT f m (AltHoldParams f)
altHoldParams =
  AltHoldParams <$> param "TRIM_THROTTLE" 0.44   -- P     I     D     IMAX
                <*> group "THR_RATE"     (pidParams 1.000 0.000 0.000 3.0)
                <*> group "THR_ACCEL"    (pidParams 0.010 0.000 0.000 5.0)
                <*> group "THR_ALT"      (pidParams 6.000 0.000 0.000 3.0)

-- | Flight control parameters.
data FlightParams f = FlightParams
  { flightRollStab  :: PIDParams f
  , flightRollRate  :: PIDParams f
  , flightPitchStab :: PIDParams f
  , flightPitchRate :: PIDParams f
  , flightYawRate   :: PIDParams f
  , flightAltHold   :: AltHoldParams f
  } deriving (Functor, Foldable, Traversable)

-- | Initialize flight parameters to their default values.
flightParams :: Monad m => ParamT f m (FlightParams f)
flightParams =                              -- P     I     D     IMAX (-IMIN)
  FlightParams <$> group "STB_RLL"  (pidParams 0.500 0.050 0.000 8.0)
               <*> group "RATE_RLL" (pidParams 0.025 0.000 0.000 5.0)
               <*> group "STB_PIT"  (pidParams 0.500 0.050 0.000 8.0)
               <*> group "RATE_PIT" (pidParams 0.025 0.000 0.000 5.0)
               <*> group "RATE_YAW" (pidParams 0.025 0.000 0.000 8.0)
               <*> group "" altHoldParams
