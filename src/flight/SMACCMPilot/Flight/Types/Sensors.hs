{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Types.Sensors where

import Ivory.Language

sensorsTypeModule :: Module
sensorsTypeModule = package "sensors_type" $ do
  defStruct (Proxy :: Proxy "sensors_result")

-- roll, pitch, yaw in radians*/
  -- float roll;
  -- float pitch;
  -- float yaw;
-- omega in radians per second
  -- float omega_x;
  -- float omega_y;
  -- float omega_z;
-- altitude in meters
  -- float baro_alt;
-- acceleration in mg 
  -- int16_t xacc;
  -- int16_t yacc;
  -- int16_t zacc;
-- time of capture
  -- uint32_t time;

[ivory|
struct sensors_result
  { valid     :: Stored IBool
  ; roll      :: Stored IFloat
  ; pitch     :: Stored IFloat
  ; yaw       :: Stored IFloat
  ; omega_x   :: Stored IFloat
  ; omega_y   :: Stored IFloat
  ; omega_z   :: Stored IFloat
  ; baro_alt  :: Stored IFloat
  ; xacc      :: Stored IFloat
  ; yacc      :: Stored IFloat
  ; zacc      :: Stored IFloat
  ; ahrs_time :: Stored Uint32
  ; baro_time :: Stored Uint32
  }
|]

