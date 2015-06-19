
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.INS.Bias.Magnetometer.Estimator
  ( monitorMagBiasEstimator
  , ivoryMagBiasEstimator
  , MagBiasEstimator(..)
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib
import qualified SMACCMPilot.INS.Bias.Magnetometer.Types as S

import Linear hiding (inv44)

data MagBiasEstimator =
  MagBiasEstimator
    { mbe_init     :: forall eff . Ivory eff ()
    , mbe_sample   :: forall eff s
                    . ConstRef s (Array 3 (Stored IFloat))
                   -> Ivory eff ()
    , mbe_output   :: forall eff s
                    . Ref s (Array 4 (Stored IFloat))
                   -> Ivory eff IFloat
    }

monitorMagBiasEstimator :: Monitor e MagBiasEstimator
monitorMagBiasEstimator = do
  n <- freshname "magbias"
  let (f, moduledef) = ivoryMagBiasEstimator (showUnique n)
  monitorModuleDef moduledef
  return f

ivoryMagBiasEstimator :: String -> (MagBiasEstimator, ModuleDef)
ivoryMagBiasEstimator n = (f, moddef)
  where

  f = MagBiasEstimator
    { mbe_init = do
        store (s ~> S.x_sq)    0
        store (s ~> S.y_sq)    0
        store (s ~> S.z_sq)    0
        store (s ~> S.x)       0
        store (s ~> S.y)       0
        store (s ~> S.z)       0
        store (s ~> S.xy)      0
        store (s ~> S.yz)      0
        store (s ~> S.xz)      0
        store (s ~> S.x_sumsq) 0
        store (s ~> S.y_sumsq) 0
        store (s ~> S.z_sumsq) 0
        store (s ~> S.m)       0
    , mbe_sample = \input -> do
        x     <- deref (input ! 0)
        y     <- deref (input ! 1)
        z     <- deref (input ! 2)
        x_sq  <- assign (x*x)
        y_sq  <- assign (y*y)
        z_sq  <- assign (z*z)
        sumsq <- assign (x_sq + y_sq + z_sq)
        (s ~> S.x_sq)    %= (+x_sq)
        (s ~> S.y_sq)    %= (+y_sq)
        (s ~> S.z_sq)    %= (+z_sq)
        (s ~> S.x)       %= (+x)
        (s ~> S.y)       %= (+y)
        (s ~> S.z)       %= (+z)
        (s ~> S.xy)      %= (+(x*y))
        (s ~> S.xz)      %= (+(x*z))
        (s ~> S.yz)      %= (+(y*z))
        (s ~> S.x_sumsq) %= (+(x*sumsq))
        (s ~> S.y_sumsq) %= (+(y*sumsq))
        (s ~> S.z_sumsq) %= (+(z*sumsq))
        (s ~> S.m)       %= (+1)
    , mbe_output = \out -> do
        xtx <- xtx_matrix s
        xty <- xty_vec s
        let (xtx_inv, failed) = inv44 xtx
            (V4 b0 b1 b2 b3) = xtx_inv !* xty
        x <- assign (b0 / 2)
        y <- assign (b1 / 2)
        z <- assign (b2 / 2)
        store (out ! 0) x
        store (out ! 1) y
        store (out ! 2) z
        store (out ! 3) (sqrt (b3 + x*x + y*y + z*z))
        return $ failed ? (0, 1)
    }

  moddef = do
    depend S.magnetometerBiasTypesModule
    defMemArea s_area

  s_area :: MemArea (Struct "mag_bias_sums")
  s_area = area (named "mag_bias_sums") Nothing
  s      = addrOf s_area

  named nn = n ++ "_" ++ nn


xtx_matrix :: Ref s (Struct "mag_bias_sums") -> Ivory eff (M44 IFloat)
xtx_matrix s = do
  x    <- deref (s ~> S.x)
  y    <- deref (s ~> S.y)
  z    <- deref (s ~> S.z)
  x_sq <- deref (s ~> S.x_sq)
  y_sq <- deref (s ~> S.y_sq)
  z_sq <- deref (s ~> S.z_sq)
  xy   <- deref (s ~> S.xy)
  xz   <- deref (s ~> S.xz)
  yz   <- deref (s ~> S.yz)
  m    <- fmap safeCast (deref (s ~> S.m))
  return $ V4 (V4 x_sq xy xz x)
              (V4 xy y_sq yz y)
              (V4 xz yz z_sq z)
              (V4 x  y  z    m)

xty_vec :: Ref s (Struct "mag_bias_sums") -> Ivory eff (V4 IFloat)
xty_vec s = do
  x_sq <- deref (s ~> S.x_sq)
  y_sq <- deref (s ~> S.y_sq)
  z_sq <- deref (s ~> S.z_sq)
  x_sumsq <- deref (s ~> S.x_sumsq)
  y_sumsq <- deref (s ~> S.y_sumsq)
  z_sumsq <- deref (s ~> S.z_sumsq)
  return (V4 x_sumsq y_sumsq z_sumsq (x_sq + y_sq + z_sq))

-- |4x4 matrix inverse.
-- Stolen from kmett's `linear` package, modified to use Ivory runtime checks
-- rather than Haskell runtime checks.
inv44 :: M44 IFloat -> (M44 IFloat, IBool)
inv44 (V4 (V4 i00 i01 i02 i03)
          (V4 i10 i11 i12 i13)
          (V4 i20 i21 i22 i23)
          (V4 i30 i31 i32 i33)) =
  let s0 = i00 * i11 - i10 * i01
      s1 = i00 * i12 - i10 * i02
      s2 = i00 * i13 - i10 * i03
      s3 = i01 * i12 - i11 * i02
      s4 = i01 * i13 - i11 * i03
      s5 = i02 * i13 - i12 * i03
      c5 = i22 * i33 - i32 * i23
      c4 = i21 * i33 - i31 * i23
      c3 = i21 * i32 - i31 * i22
      c2 = i20 * i33 - i30 * i23
      c1 = i20 * i32 - i30 * i22
      c0 = i20 * i31 - i30 * i21
      det = s0 * c5 - s1 * c4 + s2 * c3 + s3 * c2 - s4 * c1 + s5 * c0
      invDet = recip det
      inv = invDet *!!
                V4 (V4 (i11 * c5 - i12 * c4 + i13 * c3)
                       (-i01 * c5 + i02 * c4 - i03 * c3)
                       (i31 * s5 - i32 * s4 + i33 * s3)
                       (-i21 * s5 + i22 * s4 - i23 * s3))
                   (V4 (-i10 * c5 + i12 * c2 - i13 * c1)
                       (i00 * c5 - i02 * c2 + i03 * c1)
                       (-i30 * s5 + i32 * s2 - i33 * s1)
                       (i20 * s5 - i22 * s2 + i23 * s1))
                   (V4 (i10 * c4 - i11 * c2 + i13 * c0)
                       (-i00 * c4 + i01 * c2 - i03 * c0)
                       (i30 * s4 - i31 * s2 + i33 * s0)
                       (-i20 * s4 + i21 * s2 - i23 * s0))
                   (V4 (-i10 * c3 + i11 * c1 - i12 * c0)
                       (i00 * c3 - i01 * c1 + i02 * c0)
                       (-i30 * s3 + i31 * s1 - i32 * s0)
                       (i20 * s3 - i21 * s1 + i22 * s0))
  in (inv, false)
