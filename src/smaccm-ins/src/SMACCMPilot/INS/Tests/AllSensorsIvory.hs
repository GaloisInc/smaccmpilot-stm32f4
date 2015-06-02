{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module SMACCMPilot.INS.Tests.AllSensorsIvory
  ( app
  ) where

import Data.Foldable
import Data.List (intercalate)
import Data.Traversable
import System.FilePath

import Ivory.Language
import Ivory.Stdlib
import Ivory.Artifact
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)

import Numeric.Estimator.Model.Coordinate

import SMACCMPilot.INS.Ivory
import SMACCMPilot.INS.SensorFusion
import SMACCMPilot.Hardware.SensorMonitor
import qualified SMACCMPilot.Comm.Ivory.Types.AccelerometerSample as A
import qualified SMACCMPilot.Comm.Ivory.Types.GyroscopeSample     as G
import qualified SMACCMPilot.Comm.Ivory.Types.MagnetometerSample  as M
import qualified SMACCMPilot.Comm.Ivory.Types.BarometerSample     as B
import qualified SMACCMPilot.Comm.Ivory.Types.Xyz                 as XYZ
import qualified SMACCMPilot.Comm.Ivory.Types.TimeMicros          as T

import Prelude hiding (mapM, mapM_)

printf_float :: Def('[IString, IFloat] :-> ())
printf_float = importProc "printf" "stdio.h"

printf_double :: Def('[IString, IDouble] :-> ())
printf_double = importProc "printf" "stdio.h"

puts :: Def('[IString] :-> ())
puts = importProc "puts" "stdio.h"

sensorMonitor :: ([Module], [Located Artifact])
sensorMonitor = decoder $ SensorHandlers
  { sh_baro = \baro_sample -> do
      refCopy baro_buf baro_sample
      check_init init_baro $ do
        call_ pressure_measure

  , sh_mag = \mag_sample -> do
      refCopy mag_buf mag_sample
      check_init init_mag $ do
        call_ mag_measure

  , sh_gyro = \gyro_sample -> do
      refCopy gyro_buf gyro_sample

  , sh_accel = \accel_sample -> do
      refCopy accel_buf accel_sample
      check_init init_accel $ do
        now <- deref (accel_buf ~> A.time)
        prev <- deref timestamp_ref
        let dt_micros :: Sint32
            dt_micros = castDefault $ (T.unTimeMicros now) - (T.unTimeMicros prev)
            dt_seconds = (safeCast dt_micros) / 1000000.0

        call_ kalman_predict dt_seconds
        store timestamp_ref now
        call_ kalman_output

  , sh_gps = \_gps_sample -> do
      check_init 0 $ do
        -- XXX this is nonsense:
        call_ pos_measure 1.1 2.2 3.3
        call_ vel_measure 4.4 5.5 6.6
  , sh_moddef = do
      defMemArea gyro_area
      defMemArea mag_area
      defMemArea accel_area
      defMemArea baro_area
      defMemArea init_area
      defMemArea timestamp_area
      ins_moddef
  }

  where
  init_area = area "init_state" Nothing
  init_ref :: Ref Global (Stored Uint32)
  init_ref = addrOf init_area
  init_mag = 1
  init_accel = 2
  init_baro = 4
  init_done = 8
  check_init field ow = do
    i <- deref init_ref
    when (i <? 7) $ do
      store init_ref (i .| field)
    when (i ==? 7) $ do
      store init_ref init_done
      call_ kalman_init
      refCopy timestamp_ref (accel_buf ~> A.time)
      call_ kalman_output
    when (i ==? 8) ow

  xyzRefs s = fmap (s ~>) $ xyz XYZ.x XYZ.y XYZ.z

  accel_get_sample = do
    mapM deref $ xyzRefs $ accel_buf ~> A.sample

  gyro_get_sample = do
    raw <- mapM deref $ xyzRefs $ gyro_buf ~> G.sample
    let toRads deg = deg * pi / 180.0
    return $ fmap toRads raw

  mag_get_sample = do
    raw <- mapM deref $ xyzRefs $ mag_buf ~> M.sample
    let toMilligauss gauss = gauss * 1000
    return $ fmap toMilligauss raw

  baro_get_sample = do
    let toPascals mbar = mbar * 100
    fmap toPascals $ deref (baro_buf ~> B.pressure)

  timestamp_area = area "timestamp_previous" Nothing
  timestamp_ref = addrOf timestamp_area

  gyro_area = area "gyro_latest" Nothing
  gyro_buf = addrOf gyro_area

  accel_area = area "accel_latest" Nothing
  accel_buf = addrOf accel_area

  mag_area = area "mag_latest" Nothing
  mag_buf = addrOf mag_area

  baro_area = area "baro_latest" Nothing
  baro_buf = addrOf baro_area


  kalman_state :: MemArea (Struct "kalman_state")
  kalman_state = area "kalman_state" Nothing

  kalman_covariance :: MemArea (Struct "kalman_covariance")
  kalman_covariance = area "kalman_covariance" Nothing

  kalman_init :: Def ('[] :-> ())
  kalman_init = proc "kalman_init" $ body $ do
    acc <- accel_get_sample
    mag <- mag_get_sample
    pressure <- baro_get_sample
    kalmanInit (addrOf kalman_state) (addrOf kalman_covariance) acc mag pressure

  kalman_predict :: Def ('[IFloat] :-> ())
  kalman_predict = proc "kalman_predict" $ \ dt -> body $ do
    accel <- accel_get_sample
    gyro <- gyro_get_sample
    let distVector = DisturbanceVector { disturbanceGyro = gyro
                                       , disturbanceAccel = accel }
    kalmanPredict (addrOf kalman_state) (addrOf kalman_covariance) dt distVector

  kalman_output :: Def ('[]:->())
  kalman_output = proc "kalman_output" $ body $ do
    -- column 1
    timestamp <- deref timestamp_ref
    call_ printf_double "%12.6f" $ safeCast (T.unTimeMicros timestamp) / 1.0e6

    -- columns 2-4
    accel_get_sample >>= mapM_ print_float
    -- columns 5-7
    gyro_get_sample  >>= mapM_ print_float
    -- columns 8-10
    mag_get_sample   >>= mapM_ print_float
    -- column 11
    baro_get_sample  >>= print_float

    -- columns 12-15
    print_array (addrOf kalman_state ~> orient)
    -- columns 16-18
    print_array (addrOf kalman_state ~> vel)
    -- columns 19-21
    print_array (addrOf kalman_state ~> pos)
    -- columns 22-24
    print_array (addrOf kalman_state ~> gyro_bias)
    -- columns 25-27
    print_array (addrOf kalman_state ~> wind)
    -- columns 28-30
    print_array (addrOf kalman_state ~> mag_ned)
    -- columns 31-33
    print_array (addrOf kalman_state ~> mag_xyz)
    endl
    where
    print_array a = arrayMap $ \ix ->
      deref (a ! ix) >>= print_float

    print_float :: IFloat -> Ivory eff ()
    print_float v = call_ printf_float " % f" v

    endl :: Ivory eff ()
    endl = call_ puts ""

  vel_measure :: Def ('[IFloat, IFloat, IFloat] :-> ())
  vel_measure = proc "vel_measure" $ \ velN velE velD -> body $ do
    velMeasure (addrOf kalman_state) (addrOf kalman_covariance) $ ned velN velE velD

  pos_measure :: Def ('[IFloat, IFloat, IFloat] :-> ())
  pos_measure = proc "pos_measure" $ \ posN posE posD -> body $ do
    posMeasure (addrOf kalman_state) (addrOf kalman_covariance) $ ned posN posE posD

  pressure_measure :: Def ('[] :-> ())
  pressure_measure = proc "pressure_measure" $ body $ do
    pressure <- baro_get_sample
    pressureMeasure (addrOf kalman_state) (addrOf kalman_covariance) pressure

  tas_measure :: Def ('[IFloat] :-> ())
  tas_measure = proc "tas_measure" $ \ tas -> body $ do
    tasMeasure (addrOf kalman_state) (addrOf kalman_covariance) tas

  mag_measure :: Def ('[] :-> ())
  mag_measure = proc "mag_measure" $ body $ do
    mag <- mag_get_sample
    magMeasure (addrOf kalman_state) (addrOf kalman_covariance) mag


  ins_moddef :: ModuleDef
  ins_moddef = do
    defStruct (Proxy :: Proxy "kalman_state")
    defStruct (Proxy :: Proxy "kalman_covariance")
    defMemArea kalman_state
    defMemArea kalman_covariance
    incl kalman_init
    incl kalman_predict
    incl kalman_output
    incl vel_measure
    incl pos_measure
    incl pressure_measure
    incl tas_measure
    incl mag_measure
    incl printf_float
    incl puts




app :: IO ()
app = C.compile modules artifacts
  where
  (sens_modules, sens_artifacts) = sensorMonitor
  modules = sens_modules
  artifacts = makefile : sens_artifacts

  objects = [ moduleName m <.> "o" | m <- modules ] ++
    [ replaceExtension (artifactFileName a) "o" | Src a <- sens_artifacts ]

  exename = moduleName $ head modules

  makefile = Root $ artifactString "Makefile" $ unlines
    [ "CC = gcc"
    , "CFLAGS = -Wall -g -Os -std=c99 -D_BSD_SOURCE -Wno-unused-variable -I."
    , "LDLIBS = -lm"
    , "OBJS = " ++ intercalate " " objects
    , exename ++ ": $(OBJS)"
    , "clean:"
    , "\t-rm -f $(OBJS) " ++ exename
    , ".PHONY: clean"
    ]
