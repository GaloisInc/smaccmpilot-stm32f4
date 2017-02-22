{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | More-or-less transliterated from Paparazzi:
-- https://github.com/paparazzi/paparazzi/blob/master/sw/airborne/subsystems/ahrs/ahrs_float_mlkf.h
module SMACCMPilot.Flight.Control.Attitude.KalmanFilter where

import Control.Monad (forM, forM_)
import Data.Foldable
import qualified Data.Vector as V

import Ivory.Language
--import Ivory.Stdlib

import Linear
import Linear.V
import Numeric.IEEE

named :: String -> String
named nm = "att_est_" ++ nm

[ivory|
struct AhrsMlkf {
    -- state
    ltp_to_imu_quat :: Array 4 (Stored IFloat)
  ; gibbs_cor :: Array 4 (Stored IFloat)
  ; gyro_bias :: Array 3 (Stored IFloat)

    -- covariance matrix
  ; p :: Array 6 (Array 6 (Stored IFloat))
}
|]

type FloatRates = V3 IFloat

data StateVec = StateVec {
    sv_ltp_to_imu_quat :: Quaternion IFloat
  , sv_gibbs_cor :: Quaternion IFloat
  , sv_gyro_bias :: FloatRates
  }

derefStateVec :: Ref s ('Struct "AhrsMlkf") -> Ivory eff StateVec
derefStateVec ahrs = do
  sv_ltp_to_imu_quat <- derefQuat (ahrs ~> ltp_to_imu_quat)
  sv_gibbs_cor <- derefQuat (ahrs ~> gibbs_cor)
  sv_gyro_bias <- derefV3 (ahrs ~> gyro_bias)
  return StateVec {..}

storeStateVec :: Ref s ('Struct "AhrsMlkf") -> StateVec -> Ivory eff ()
storeStateVec ahrs StateVec {..} = do
  storeQuat (ahrs ~> ltp_to_imu_quat) sv_ltp_to_imu_quat
  storeQuat (ahrs ~> gibbs_cor) sv_gibbs_cor
  storeV3 (ahrs ~> gyro_bias) sv_gyro_bias

derefQuat :: Ref s ('Array 4 ('Stored IFloat)) -> Ivory eff (Quaternion IFloat)
derefQuat ref = do
  [i, x, y, z] <- forM [0, 1, 2, 3] $ \ix -> deref (ref ! ix)
  return (Quaternion i (V3 x y z))

storeQuat
  :: Ref s ('Array 4 ('Stored IFloat)) -> Quaternion IFloat -> Ivory eff ()
storeQuat ref (Quaternion i (V3 x y z)) = do
  store (ref ! 0) i
  store (ref ! 1) x
  store (ref ! 2) y
  store (ref ! 3) z

derefV3 :: Ref s ('Array 3 ('Stored IFloat)) -> Ivory eff (V3 IFloat)
derefV3 ref = do
  [x, y, z] <- forM [0, 1, 2] $ \ix -> deref (ref ! ix)
  return (V3 x y z)

storeV3 :: Ref s ('Array 3 ('Stored IFloat)) -> V3 IFloat -> Ivory eff ()
storeV3 ref (V3 x y z) = do
  store (ref ! 0) x
  store (ref ! 1) y
  store (ref ! 2) z

-- row-major 6x6 matrix
type CovMat = V $(6) (V $(6) IFloat)

derefCovMat :: Ref s ('Struct "AhrsMlkf") -> Ivory eff CovMat
derefCovMat ahrs = do
  rs <- forM [0, 1, 2, 3, 4, 5] $ \r -> do
    cs <- forM [0, 1, 2, 3, 4, 5] $ \c ->
      deref (((ahrs ~> p) ! r) ! c)
    return (V (V.fromList cs))
  return (V (V.fromList rs))

storeCovMat :: Ref s ('Struct "AhrsMlkf") -> CovMat -> Ivory eff ()
storeCovMat ahrs covMat = do
  forM_ (zip [0, 1, 2, 3, 4, 5] (toList covMat)) $ \(r, rv) ->
    forM_ (zip [0, 1, 2, 3, 4, 5] (toList rv)) $ \(c, cv) ->
      store (((ahrs ~> p) ! r) ! c) cv

--------------------------------------------------------------------------------
-- Constants, to be turned into config values later

ahrs_h_x, ahrs_h_y, ahrs_h_z :: IFloat
ahrs_h_x = 0
ahrs_h_y = 0
ahrs_h_z = 0

flt_min :: IFloat
flt_min = fromRational (toRational (minNormal :: Float))

{-
extern void ahrs_mlkf_init(void);

// skipping for now because our IMU and body should be aligned
extern void ahrs_mlkf_set_body_to_imu(struct OrientationReps *body_to_imu);
// skipping for now because our IMU and body should be aligned
extern void ahrs_mlkf_set_body_to_imu_quat(struct FloatQuat *q_b2i);

extern bool ahrs_mlkf_align(struct FloatRates *lp_gyro, struct FloatVect3 *lp_accel,
                              struct FloatVect3 *lp_mag);
extern void ahrs_mlkf_propagate(struct FloatRates *gyro, float dt);
extern void ahrs_mlkf_update_accel(struct FloatVect3 *accel);
extern void ahrs_mlkf_update_mag(struct FloatVect3 *mag);
extern void ahrs_mlkf_update_mag_2d(struct FloatVect3 *mag);
extern void ahrs_mlkf_update_mag_full(struct FloatVect3 *mag);
-}

quatRotId :: Quaternion IFloat
quatRotId = Quaternion 1 zero

ahrs_mlkf_init :: Ref s ('Struct "AhrsMlkf") -> Ivory eff ()
ahrs_mlkf_init ahrs = do
  let sv0 = StateVec {
              sv_ltp_to_imu_quat = quatRotId
            , sv_gibbs_cor = quatRotId
            , sv_gyro_bias = zero
            }
  let p0 = scaled (V (V.fromList [p0_a, p0_a, p0_a, p0_b, p0_b, p0_b]))
      p0_a = 1
      p0_b = 1e-4
  storeStateVec ahrs sv0
  storeCovMat ahrs p0

-- | Get a roll and pitch quaternion from an accelerometer
-- measurement. If the IMU is directly upside down, we don't want the
-- cross product of the vectors to be zero
ahrs_float_get_quat_from_accel :: V3 IFloat -> Quaternion IFloat
ahrs_float_get_quat_from_accel accel = signorm q
  where
    q = Quaternion qi (V3 qx qy qz)
    (V3 x y z) = signorm accel
    deg180 = abs (z-1) <? 5 * flt_min
    qi = deg180 ? (0, 1 - z)
    qx = deg180 ? (1, (-y))
    qy = deg180 ? (0, x)
    qz = 0

{-
static inline void ahrs_float_get_quat_from_accel_mag(struct FloatQuat *q, struct FloatVect3 *accel,
    struct FloatVect3 *mag)
{

  /* the quaternion representing roll and pitch from acc measurement */
  struct FloatQuat q_a;
  ahrs_float_get_quat_from_accel(&q_a, accel);

  /* and rotate to horizontal plane using the quat from above */
  struct FloatRMat rmat_phi_theta;
  float_rmat_of_quat(&rmat_phi_theta, &q_a);
  struct FloatVect3 mag_ltp;
  float_rmat_transp_vmult(&mag_ltp, &rmat_phi_theta, mag);

  /* heading from mag -> make quaternion to rotate around ltp z axis*/
  struct FloatQuat q_m;

  /* dot([mag_n.x, mag_n.x, 0], [AHRS_H_X, AHRS_H_Y, 0]) */
  float dot = mag_ltp.x * AHRS_H_X + mag_ltp.y * AHRS_H_Y;

  /* |v1||v2| */
  float norm2 = sqrtf(SQUARE(mag_ltp.x) + SQUARE(mag_ltp.y))
                * sqrtf(SQUARE(AHRS_H_X) + SQUARE(AHRS_H_Y));

  // catch 180deg case
  if (ABS(norm2 + dot) < 5 * FLT_MIN) {
    QUAT_ASSIGN(q_m, 0.0, 0.0, 0.0, 1.0);
  } else {
    /* q_xyz = cross([mag_n.x, mag_n.y, 0], [AHRS_H_X, AHRS_H_Y, 0]) */
    q_m.qx = 0.0;
    q_m.qy = 0.0;
    q_m.qz = mag_ltp.x * AHRS_H_Y - mag_ltp.y * AHRS_H_X;
    q_m.qi = norm2 + dot;
    float_quat_normalize(&q_m);
  }

  // q_ltp2imu = q_a * q_m
  // and wrap and normalize
  float_quat_comp_norm_shortest(q, &q_m, &q_a);
}
-}


-- | Wrap a quaternion around to the shortest angle
wrapShortest :: Quaternion IFloat -> Quaternion IFloat
wrapShortest (Quaternion i (V3 x y z)) = q'
  where f n = (i <? 0) ? ((-n), n)
        q' = Quaternion (f i) (V3 (f x) (f y) (f z))

-- | Because we don't have a RealFloat instance for IFloat :(
quatMult :: Quaternion IFloat -> Quaternion IFloat -> Quaternion IFloat
quatMult (Quaternion s1 v1) (Quaternion s2 v2) =
  Quaternion (s1*s2 - (v1 `dot` v2)) ((v1 `cross` v2) + s1*^v2 + s2*^v1)

quatConj :: Conjugate a => Quaternion a -> Quaternion a
quatConj (Quaternion e v) = Quaternion (conjugate e) (negate v)

instance Conjugate IFloat
instance TrivialConjugate IFloat

quatRotate :: Quaternion IFloat -> V3 IFloat -> V3 IFloat
quatRotate q v = ijk where
  Quaternion _ ijk = q `quatMult` Quaternion 0 v `quatMult` quatConj q

float_quat_comp_norm_shortest
  :: Quaternion IFloat -> Quaternion IFloat -> Quaternion IFloat
float_quat_comp_norm_shortest a2b b2c =
  signorm (wrapShortest (a2b `quatMult` b2c))

-- | Get an orientation directly from mag and accel
ahrs_float_get_quat_from_accel_mag
  :: V3 IFloat -> V3 IFloat -> Quaternion IFloat
ahrs_float_get_quat_from_accel_mag accel mag =
    float_quat_comp_norm_shortest q_a q_m
  where
    q_a = ahrs_float_get_quat_from_accel accel
    q_m = signorm (Quaternion q_mi (V3 q_mx q_my q_mz))
    (V3 mag_ltp_x mag_ltp_y _) = quatRotate q_a mag
    v1 = V3 mag_ltp_x mag_ltp_y 0
    v2 = V3 ahrs_h_x ahrs_h_y 0
    mag_dot = v1 `dot` v2
    norm2 = norm v1 * norm v2
    deg180 = abs (norm2 + mag_dot) <? 5 * flt_min
    q_mi = deg180 ? (0, norm2 + mag_dot)
    q_mx = 0
    q_my = 0
    q_mz = deg180 ? (1, mag_ltp_x * ahrs_h_y - mag_ltp_y * ahrs_h_x)
