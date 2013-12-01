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

-- | Altitude controller parameters.
data AltitudeParams f = AltitudeParams
  { altitudeRateThrust :: PIDParams f
  , altitudePosition   :: PIDParams f
  , altitudeUI         :: ThrUIParams f
  } deriving (Functor, Foldable, Traversable)

-- | altitudeParams tuned for AR Drone. Not tested with 3DR Quad yet.
altitudeParams :: Monad m => ParamT f m (AltitudeParams f)
altitudeParams =                              -- P     I     D     IMAX
  AltitudeParams <$> group "ALT_RATE" (pidParams 0.140 0.001 0.050 0.8)
                 <*> group "ALT_POS"  (pidParams 0.100 0.000 0.000 5.0)
                 <*> group "ALT_UI"   (thrUIParams 1.0 0.25)

-- | PID controller parameters.
data ThrUIParams  f = ThrUIParams 
  { thrUIsens :: Param f  -- Meters per second at full scale
  , thrUIdead :: Param f  -- Deadband as a fraction of full scale (0..1)
  } deriving (Functor, Foldable, Traversable)

thrUIParams :: Monad m => Float -> Float
            -> ParamT f m (ThrUIParams f)
thrUIParams s d =
  ThrUIParams <$> param "SENS" s
              <*> param "DEAD" d

-- | Flight control parameters.
data FlightParams f = FlightParams
  { flightRollStab  :: PIDParams f
  , flightRollRate  :: PIDParams f
  , flightPitchStab :: PIDParams f
  , flightPitchRate :: PIDParams f
  , flightYawRate   :: PIDParams f
  , flightAltitude  :: AltitudeParams f
  } deriving (Functor, Foldable, Traversable)

-- | Initialize flight parameters to their default values.
flightParams :: Monad m => ParamT f m (FlightParams f)
flightParams =                              -- P     I     D     IMAX (-IMIN)
  FlightParams <$> group "STB_RLL"  (pidParams 2.000 0.000 0.000 8.0)
               <*> group "RATE_RLL" (pidParams 0.100 0.000 0.000 5.0)
               <*> group "STB_PIT"  (pidParams 2.000 0.000 0.000 8.0)
               <*> group "RATE_PIT" (pidParams 0.105 0.000 0.000 5.0)
               <*> group "RATE_YAW" (pidParams 0.305 0.000 0.000 8.0)
               <*> group "" altitudeParams
