{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.INS.Bias.Magnetometer.Tower
  ( calcMagBiasTower
  , calcMagBiasTower'
  , magCalibrate
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import SMACCMPilot.INS.Bias.Calibration
import SMACCMPilot.INS.Bias.Magnetometer.Diversity
import SMACCMPilot.INS.Bias.Magnetometer.Types (magnetometerBiasTypesModule)
import SMACCMPilot.Time
import qualified SMACCMPilot.Comm.Ivory.Types.MagnetometerSample  as M
import qualified SMACCMPilot.Comm.Ivory.Types.XyzCalibration      as C
import qualified SMACCMPilot.Comm.Ivory.Types.Xyz                 as XYZ

calcMagBiasTower :: ChanOutput (Struct "magnetometer_sample")
                  -> Tower e (ChanOutput (Struct "xyz_calibration"))
calcMagBiasTower m = do
  cal <- channel
  p <- period (Milliseconds 1000)
  calcMagBiasTower' m (fst cal) p
  return (snd cal)

calcMagBiasTower' :: ChanOutput (Struct "magnetometer_sample")
                   -> ChanInput  (Struct "xyz_calibration")
                   -> ChanOutput (Stored ITime)
                   -> Tower e ()
calcMagBiasTower' m c newoutput = do
  towerModule magnetometerBiasTypesModule
  towerModule magDiversityHelpersModule
  monitor "calcMagBias" $ do
    mbe <- monitorDiverseMagBiasEstimator
    handler systemInit "init" $ callback $ const $ do
      mbe_init mbe

    handler m "mag" $ callback $ \m_samp -> do
      m_axes <- mkMagSample m_samp
      mbe_sample mbe m_axes

    handler newoutput "output" $ do
      e <- emitter c 1
      callbackV $ \t -> do
        biases <- local izero
        progress <- mbe_output mbe biases
        cal <- mkCalibration (constRef biases) progress t
        emit e cal

mkMagSample :: (GetAlloc eff ~ Scope s)
             => ConstRef s' (Struct "magnetometer_sample")
             -> Ivory eff (ConstRef (Stack s) (Array 3 (Stored IFloat)))
mkMagSample sample = do
  x <- deref ((sample ~> M.sample) ~> XYZ.x)
  y <- deref ((sample ~> M.sample) ~> XYZ.y)
  z <- deref ((sample ~> M.sample) ~> XYZ.z)
  fmap constRef $ local $ iarray [ ival x, ival y, ival z ]

mkCalibration :: (GetAlloc eff ~ Scope s)
              => ConstRef s' (Array 4 (Stored IFloat))
              -> IFloat
              -> ITime
              -> Ivory eff (ConstRef (Stack s) (Struct "xyz_calibration"))
mkCalibration cal progress time = do
  x <- deref (cal ! 0)
  y <- deref (cal ! 1)
  z <- deref (cal ! 2)
  fullscale <- deref (cal ! 3)
  scalefactor <- assign ((fullscale ==? 0) ? (1, (1 / fullscale)))
  fmap constRef $ local $ istruct
    [ C.valid    .= ival (progress >=? 1.0)
    , C.progress .= ival progress
    , C.bias     .= istruct
        [ XYZ.x  .= ival x
        , XYZ.y  .= ival y
        , XYZ.z  .= ival z
        ]
    , C.scale    .= istruct
        [ XYZ.x  .= ival scalefactor
        , XYZ.y  .= ival scalefactor
        , XYZ.z  .= ival scalefactor
        ]
    , C.time     .= ival (timeMicrosFromITime time)
    ]

magCalibrate :: Calibrate (Struct "magnetometer_sample")
magCalibrate = Calibrate aux
  where
  aux samp cal = do
    out <- local izero
    refCopy out samp
    v <- deref (cal ~> C.valid)
    when v $ do
      bx <- deref ((cal ~> C.bias) ~> XYZ.x)
      by <- deref ((cal ~> C.bias) ~> XYZ.y)
      bz <- deref ((cal ~> C.bias) ~> XYZ.z)
      sx <- deref ((cal ~> C.scale) ~> XYZ.x)
      sy <- deref ((cal ~> C.scale) ~> XYZ.y)
      sz <- deref ((cal ~> C.scale) ~> XYZ.z)
      let c bias scale uncal = (uncal - bias) * scale
      ((out ~> M.sample) ~> XYZ.x) %= c bx sx
      ((out ~> M.sample) ~> XYZ.y) %= c by sy
      ((out ~> M.sample) ~> XYZ.z) %= c bz sz
      store (out ~> M.calibrated) true
    return (constRef out)
