{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.List

import Ivory.Language
import Ivory.Serialize
import Ivory.Artifact
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)

import Numeric.Estimator.Model.Coordinate
import Numeric.Estimator.Model.SensorFusion

import SMACCMPilot.INS.Ivory
import SMACCMPilot.Hardware.SensorMonitor

kalman_state :: MemArea (Struct "kalman_state")
kalman_state = area "kalman_state" Nothing

kalman_covariance :: MemArea (Struct "kalman_covariance")
kalman_covariance = area "kalman_covariance" Nothing

kalman_init :: Def ('[IFloat, IFloat, IFloat, IFloat, IFloat, IFloat, IFloat] :-> ())
kalman_init = proc "kalman_init" $ \ accX accY accZ magX magY magZ pressure -> body $ do
  let acc = xyz accX accY accZ
  let mag = xyz magX magY magZ
  kalmanInit (addrOf kalman_state) (addrOf kalman_covariance) acc mag pressure

kalman_predict :: Def ('[IFloat, IFloat, IFloat, IFloat, IFloat, IFloat, IFloat] :-> ())
kalman_predict = proc "kalman_predict" $ \ dt dax day daz dvx dvy dvz -> body $ do
  let distVector = DisturbanceVector { disturbanceGyro = xyz dax day daz
                                     , disturbanceAccel = xyz dvx dvy dvz }
  kalmanPredict (addrOf kalman_state) (addrOf kalman_covariance) dt distVector

vel_measure :: Def ('[IFloat, IFloat, IFloat] :-> ())
vel_measure = proc "vel_measure" $ \ velN velE velD -> body $ do
  velMeasure (addrOf kalman_state) (addrOf kalman_covariance) $ ned velN velE velD

pos_measure :: Def ('[IFloat, IFloat, IFloat] :-> ())
pos_measure = proc "pos_measure" $ \ posN posE posD -> body $ do
  posMeasure (addrOf kalman_state) (addrOf kalman_covariance) $ ned posN posE posD

pressure_measure :: Def ('[IFloat] :-> ())
pressure_measure = proc "pressure_measure" $ \ pressure -> body $ do
  pressureMeasure (addrOf kalman_state) (addrOf kalman_covariance) pressure

tas_measure :: Def ('[IFloat] :-> ())
tas_measure = proc "tas_measure" $ \ tas -> body $ do
  tasMeasure (addrOf kalman_state) (addrOf kalman_covariance) tas

mag_measure :: Def ('[IFloat, IFloat, IFloat] :-> ())
mag_measure = proc "mag_measure" $ \ magX magY magZ -> body $ do
  magMeasure (addrOf kalman_state) (addrOf kalman_covariance) $ xyz magX magY magZ


ins_module :: Module
ins_module = package "smaccm_ins" $ do
  defStruct (Proxy :: Proxy "kalman_state")
  defStruct (Proxy :: Proxy "kalman_covariance")
  defMemArea kalman_state
  defMemArea kalman_covariance
  incl kalman_init
  incl kalman_predict
  incl vel_measure
  incl pos_measure
  incl pressure_measure
  incl tas_measure
  incl mag_measure

sensorMonitor :: [Module]
sensorMonitor = decoder $ SensorHandlers
  { sh_baro = \_baro_sample -> do
      call_ pressure_measure 0.0 -- XXX FIXME
  , sh_mag = \_mag_sample -> do
      call_ mag_measure 1.2 2.3 3.4 -- XXX FIXME
  , sh_gyro = \gyro_sample -> do
      refCopy gyro_buf gyro_sample
  , sh_accel = \_accel_sample -> do
      -- XXX get gyro_buf
      call_ kalman_predict 1.0 2.2 3.3 4.4 5.5 6.6 7.7
  , sh_gps = \_gps_sample -> do
      call_ pos_measure 1.1 2.2 3.3
      call_ vel_measure 4.4 5.5 6.6
  , sh_moddef = do
      defMemArea gyro_area
      depend ins_module
  }

  where
  gyro_area = area "gyro" Nothing
  gyro_buf = addrOf gyro_area

main :: IO ()
main = C.compile modules artifacts
  where
  modules = [ins_module] ++ sensorMonitor
  artifacts = [Root makefile] ++ serializeArtifacts

  makefile = artifactString "Makefile" $ unlines [
      "CC = gcc",
      "CFLAGS = -Wall -Os -Wno-unused-variable -I.",
      "LDLIBS = -lm",
      "OBJS = " ++ intercalate " " [ moduleName m ++ ".o" | m <- modules ],
      "test: $(OBJS)",
      "clean:",
      "\t-rm -f $(OBJS)",
      ".PHONY: clean"
    ]
