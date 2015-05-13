{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Data.List
import Ivory.Artifact
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)
import Ivory.Language
import Numeric.Estimator.Model.Coordinate
import Numeric.Estimator.Model.SensorFusion
import qualified Paths_smaccm_ins as P
import SMACCMPilot.INS.Ivory

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
  let distVector = DisturbanceVector { disturbanceGyro = xyz dax day daz, disturbanceAccel = xyz dvx dvy dvz }
  kalmanPredict (addrOf kalman_state) (addrOf kalman_covariance) dt distVector

vel_measure :: Def ('[IFloat, IFloat, IFloat] :-> ())
vel_measure = proc "vel_measure" $ \ velN velE velD -> body $ velMeasure (addrOf kalman_state) (addrOf kalman_covariance) $ ned velN velE velD

pos_measure :: Def ('[IFloat, IFloat, IFloat] :-> ())
pos_measure = proc "pos_measure" $ \ posN posE posD -> body $ posMeasure (addrOf kalman_state) (addrOf kalman_covariance) $ ned posN posE posD

pressure_measure :: Def ('[IFloat] :-> ())
pressure_measure = proc "pressure_measure" $ \ pressure -> body $ pressureMeasure (addrOf kalman_state) (addrOf kalman_covariance) pressure

tas_measure :: Def ('[IFloat] :-> ())
tas_measure = proc "tas_measure" $ \ tas -> body $ tasMeasure (addrOf kalman_state) (addrOf kalman_covariance) tas

mag_measure :: Def ('[IFloat, IFloat, IFloat] :-> ())
mag_measure = proc "mag_measure" $ \ magX magY magZ -> body $ magMeasure (addrOf kalman_state) (addrOf kalman_covariance) $ xyz magX magY magZ

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

main :: IO ()
main = C.compile modules artifacts
  where
  modules = [ins_module]
  artifacts = [ Root $ makefile
              , Src  $ artifactCabalFile P.getDataDir "tests/psas.c"
              , Incl $ artifactCabalFile P.getDataDir "tests/psas-packet.h"
              ]
  makefile = artifactString "Makefile" $ unlines [
      "CC = gcc",
      "CFLAGS = -Wall -Os -Wno-unused-variable",
      "LDLIBS = -lm",
      "OBJS = psas.o " ++ intercalate " " [ moduleName m ++ ".o" | m <- modules ],
      "psas: $(OBJS)",
      "clean:",
      "\t-rm -f $(OBJS)",
      ".PHONY: clean"
    ]
