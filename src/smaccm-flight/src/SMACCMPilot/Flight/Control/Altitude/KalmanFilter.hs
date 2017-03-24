{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

-- | More-or-less transliterated from Paparazzi:
-- https://github.com/paparazzi/paparazzi/blob/master/sw/airborne/subsystems/ins/vf_extended_float.c
module SMACCMPilot.Flight.Control.Altitude.KalmanFilter (
    StateVec
  , derefStateVec
  , r_baro
  , r_alt
  , r_offset
  , AltState(..)
  , AltEstimator(..)
  , monitorAltEstimator
  ) where

import Control.Monad (forM, forM_)
import Data.Foldable (toList)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import Linear

import qualified SMACCMPilot.Comm.Ivory.Types.AltControlDebug as A

named :: String -> String
named nm = "alt_est_" ++ nm

[ivory|
struct VffExtended {
    -- state vector
    vff_z           :: IFloat -- z-position estimate in m (NED, z-down)
  ; vff_zdot        :: IFloat -- z-velocity estimate in m/s (NED, z-down)
  ; vff_bias        :: IFloat -- accel bias estimate in m/s^2
  ; vff_offset      :: IFloat -- baro offset estimate

    -- covariance matrix
  ; vff_p :: Array 4 (Array 4 (Stored IFloat))
}
|]

type StateVec = V4 IFloat

derefStateVec :: Ref s ('Struct "VffExtended") -> Ivory eff (V4 IFloat)
derefStateVec vff = do
  z <- deref (vff ~> vff_z)
  zdot <- deref (vff ~> vff_zdot)
  bias <- deref (vff ~> vff_bias)
  offset <- deref (vff ~> vff_offset)
  return (V4 z zdot bias offset)

storeStateVec :: Ref s ('Struct "VffExtended") -> StateVec -> Ivory eff ()
storeStateVec vff (V4 z zdot bias offset) =
  unless (isnan z .|| isnan zdot .|| isnan bias .|| isnan offset) $ do
    store (vff ~> vff_z) z
    store (vff ~> vff_zdot) zdot
    store (vff ~> vff_bias) bias
    store (vff ~> vff_offset) offset

-- row-major 4x4 matrix
type CovMat = M44 IFloat

derefCovMat :: Ref s ('Struct "VffExtended") -> Ivory eff (V4 (V4 IFloat))
derefCovMat vff = do
  [r0, r1, r2, r3] <- forM [0, 1, 2, 3] $ \r -> do
    [c0, c1, c2, c3] <- forM [0, 1, 2, 3] $ \c ->
      deref (((vff ~> vff_p) ! r) ! c)
    return (V4 c0 c1 c2 c3)
  return (V4 r0 r1 r2 r3)

storeCovMat :: Ref s ('Struct "VffExtended") -> CovMat -> Ivory eff ()
storeCovMat vff covMat =
  forM_ (zip [0, 1, 2, 3] (toList covMat)) $ \(r, rv) ->
    forM_ (zip [0, 1, 2, 3] (toList rv)) $ \(c, cv) ->
      store (((vff ~> vff_p) ! r) ! c) cv

{-
extern void vff_update_offset(float offset);
extern void vff_update_baro_conf(float z_meas, float conf);
extern void vff_update_z_conf(float z_meas, float conf);
extern void vff_update_vz_conf(float vz_meas, float conf);
extern void vff_realign(float z_meas);
-}

-- constants from paparazzi
vFF_EXTENDED_ACCEL_NOISE :: IFloat
vFF_EXTENDED_ACCEL_NOISE = 0.5

qbiasbias :: IFloat
qbiasbias = 1e-7

qoffoff :: IFloat
qoffoff = 1e-4

r_baro :: IFloat
r_baro = 1

r_alt :: IFloat
r_alt = 0.1

r_offset :: IFloat
r_offset = 1

vff_init_zero :: Ref s ('Struct "VffExtended") -> Def ('[] ':-> ())
vff_init_zero vff = voidProc (named "vff_init_zero") $
  body $ call_ (vff_init vff) 0 0 0 0

vff_init
  :: Ref s ('Struct "VffExtended")
  -> Def ('[IFloat, IFloat, IFloat, IFloat] ':-> ())
vff_init vff = voidProc (named "vff_init") $
  \z zdot bias offset -> body $ do
    store (vff ~> vff_z) z
    store (vff ~> vff_zdot) zdot
    store (vff ~> vff_bias) bias
    store (vff ~> vff_offset) offset

-- Propagate the filter in time.
--
-- F = [ 1 dt -dt^2/2 0
--       0  1 -dt     0
--       0  0   1     0
--       0  0   0     1 ];
--
-- B = [ dt^2/2 dt 0 0]';
--
-- Q = [ Qzz   0          0         0
--       0     Qzdotzdot  0         0
--       0     0          Qbiasbias 0
--       0     0     0    0         Qoffoff ];
--
-- Qzz =  VFF_EXTENDED_ACCEL_NOISE * DT_VFILTER * DT_VFILTER / 2.
-- Qzdotzdot =  VFF_EXTENDED_ACCEL_NOISE * DT_VFILTER
--
-- Xk1 = F * Xk0 + B * (accel + 9.81);
--
-- Pk1 = F * Pk0 * F' + Q;
vff_propagate
  :: Ref s ('Struct "VffExtended") -> Def ('[IFloat, IFloat] ':-> ())
vff_propagate vff = voidProc (named "vff_propagate") $
  \accel dt -> body $ do
    xk0 <- derefStateVec vff
    pk0 <- derefCovMat vff

    let f = V4 (V4 1 dt (-dt^(2::Int)/2) 0)
               (V4 0  1            (-dt) 0)
               (V4 0  0                1 0)
               (V4 0  0                0 1)
        b = V4 (dt^(2::Int)/2) dt 0 0
        q = scaled (V4 qzz qzdotzdot qbiasbias qoffoff)
        qzz = vFF_EXTENDED_ACCEL_NOISE * dt * dt / 2
        qzdotzdot = vFF_EXTENDED_ACCEL_NOISE * dt
        xk1 = (f !* xk0) ^+^ (b ^* (accel + 9.81))
        pk1 = f !*! pk0 !*! (transpose f) !+! q

    storeStateVec vff xk1
    storeCovMat vff pk1

-- Shared code for all measurements. Rather than separate functions as
-- in Paparazzi, we can use a single function parameterized by choice
-- of H matrix. Since H is a compile-time argument, the constant
-- folding pass should leave very similar code to the hand-unrolled
-- Paparazzi stuff.
--
-- // state residual
-- y = rangemeter - H * Xm;
-- // covariance residual
-- S = H*Pm*H' + R;
-- // kalman gain
-- K = Pm*H'*inv(S);
-- // update state
-- Xp = Xm + K*y;
-- // update covariance
-- Pp = Pm - K*H*Pm;
update_state_conf
  :: V4 IFloat
  -> Ref s ('Struct "VffExtended")
  -> IFloat
  -> IFloat
  -> Ivory eff ()
update_state_conf h vff z_meas conf = do
  xm <- derefStateVec vff
  pm <- derefCovMat vff

  let y :: IFloat
      y = z_meas - (h `dot` xm)
      s :: IFloat
      s = (h *! pm) `dot` h + conf
      k :: V4 IFloat
      k = (pm !* h) ^* (1 / s)
      xp = xm + (k ^* y)
      pp = pm !-! (k `outer` h !*! pm)

  storeStateVec vff xp
  storeCovMat vff pp

-- Update sensor "with" offset (baro).
--
-- H = [1 0 0 -1];
-- // state residual
-- y = rangemeter - H * Xm;
-- // covariance residual
-- S = H*Pm*H' + R;
-- // kalman gain
-- K = Pm*H'*inv(S);
-- // update state
-- Xp = Xm + K*y;
-- // update covariance
-- Pp = Pm - K*H*Pm;
update_baro_conf
  :: Ref s ('Struct "VffExtended") -> Def ('[IFloat, IFloat] ':-> ())
update_baro_conf vff = voidProc (named "update_baro_conf") $
  \z_meas conf -> body $ update_state_conf h vff z_meas conf
    where h = V4 1 0 0 (-1)

-- Update sensor "without" offset (gps, sonar)
--
-- H = [1 0 0 0];
-- // state residual
-- y = rangemeter - H * Xm;
-- // covariance residual
-- S = H*Pm*H' + R;
-- // kalman gain
-- K = Pm*H'*inv(S);
-- // update state
-- Xp = Xm + K*y;
-- // update covariance
-- Pp = Pm - K*H*Pm;
update_alt_conf
  :: Ref s ('Struct "VffExtended") -> Def ('[IFloat, IFloat] ':-> ())
update_alt_conf vff = voidProc (named "update_alt_conf") $
  \z_meas conf -> body $ update_state_conf h vff z_meas conf
    where h = V4 1 0 0 0

-- Update sensor offset (baro).
--
-- H = [0 0 0 1];
-- // state residual
-- y = rangemeter - H * Xm;
-- // covariance residual
-- S = H*Pm*H' + R;
-- // kalman gain
-- K = Pm*H'*inv(S);
-- // update state
-- Xp = Xm + K*y;
-- // update covariance
-- Pp = Pm - K*H*Pm;
update_offset_conf
  :: Ref s ('Struct "VffExtended") -> Def ('[IFloat, IFloat] ':-> ())
update_offset_conf vff = voidProc (named "update_offset_conf") $
  \offset conf -> body $ update_state_conf h vff offset conf
    where h = V4 0 0 0 1

-- Paparazzi's comment for this one makes it sound like the
-- measurement noise (R) is a constant, but their code takes it as an
-- argument. I'm going to follow the code...
--
-- H = [0 1 0 0];
-- R = 0.1;
-- // state residual
-- yd = vzd - H * Xm;
-- // covariance residual
-- S = H*Pm*H' + R;
-- // kalman gain
-- K = Pm*H'*inv(S);
-- // update state
-- Xp = Xm + K*yd;
-- // update covariance
-- Pp = Pm - K*H*Pm;
update_vz_conf
  :: Ref s ('Struct "VffExtended") -> Def ('[IFloat, IFloat] ':-> ())
update_vz_conf vff = voidProc (named "update_vz_conf") $
  \vz conf -> body $ update_state_conf h vff vz conf
    where h = V4 0 1 0 0

data AltState =
  AltState
    { as_z :: IFloat
    , as_zdot :: IFloat
    , as_accel_bias :: IFloat
    , as_baro_offset :: IFloat
    }

data AltEstimator =
  AltEstimator
    { ae_init             :: forall eff . Ivory eff ()
    , ae_propagate        :: forall eff . IFloat -> IFloat -> Ivory eff ()
    , ae_measure_offset   :: forall eff . IFloat -> IFloat -> Ivory eff ()
    , ae_measure_absolute :: forall eff . IFloat -> IFloat -> Ivory eff ()
    , ae_update_offset    :: forall eff . IFloat -> IFloat -> Ivory eff ()
    , ae_measure_velocity :: forall eff . IFloat -> IFloat -> Ivory eff ()
    , ae_state            :: forall eff . Ivory eff AltState
    , ae_write_debug      :: forall eff s . Ref s ('Struct "alt_control_debug")
                                         -> Ivory eff ()
    }

monitorAltEstimator :: Monitor e AltEstimator
monitorAltEstimator = do
  vff_state <- state (named "vff_state")
  monitorModuleDef $ do
    defStruct (Proxy :: Proxy "VffExtended")
    incl (vff_init_zero      vff_state)
    incl (vff_init           vff_state)
    incl (vff_propagate      vff_state)
    incl (update_baro_conf   vff_state)
    incl (update_alt_conf    vff_state)
    incl (update_offset_conf vff_state)
    incl (update_vz_conf     vff_state)
  return AltEstimator
    { ae_init = call_ (vff_init_zero vff_state)
    , ae_propagate = call_ (vff_propagate vff_state)
    , ae_measure_offset = call_ (update_baro_conf vff_state)
    , ae_measure_absolute = call_ (update_alt_conf vff_state)
    , ae_update_offset = call_ (update_offset_conf vff_state)
    , ae_measure_velocity = call_ (update_vz_conf vff_state)
    , ae_state = do
        V4 as_z as_zdot as_accel_bias as_baro_offset <- derefStateVec vff_state
        return AltState{..}
    , ae_write_debug = \dbg -> do
        V4 as_z as_zdot _ _ <- derefStateVec vff_state
        store (dbg ~> A.alt_est) as_z
        store (dbg ~> A.alt_rate_est) as_zdot
    }
