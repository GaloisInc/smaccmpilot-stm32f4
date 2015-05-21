{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module SMACCMPilot.INS.Tests.AllSensorsIvory
  ( app
  ) where

import Data.List (intercalate)
import System.FilePath

import Ivory.Language
import Ivory.Stdlib
import Ivory.Artifact
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)

import Numeric.Estimator.Model.Coordinate
import Numeric.Estimator.Model.SensorFusion

import SMACCMPilot.INS.Ivory
import SMACCMPilot.Hardware.SensorMonitor
import qualified SMACCMPilot.Comm.Ivory.Types.AccelerometerSample as A
import qualified SMACCMPilot.Comm.Ivory.Types.GyroscopeSample     as G
import qualified SMACCMPilot.Comm.Ivory.Types.MagnetometerSample  as M
import qualified SMACCMPilot.Comm.Ivory.Types.BarometerSample     as B
import qualified SMACCMPilot.Comm.Ivory.Types.Xyz                 as XYZ
import qualified SMACCMPilot.Comm.Ivory.Types.TimeMicros          as T


printf_float :: Def('[IString, IFloat] :-> ())
printf_float = importProc "printf" "stdio.h"

puts :: Def('[IString] :-> ())
puts = importProc "puts" "stdio.h"

sensorMonitor :: ([Module], [Located Artifact])
sensorMonitor = decoder $ SensorHandlers
  { sh_baro = \baro_sample -> do
      refCopy baro_buf baro_sample
      check_init init_baro $ do
        p <- baro_get_sample baro_buf
        call_ pressure_measure p

  , sh_mag = \mag_sample -> do
      refCopy mag_buf mag_sample
      check_init init_mag $ do
        (mx, my, mz) <- mag_get_sample mag_buf
        call_ mag_measure mx my mz

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

        (ax, ay, az) <- accel_get_sample accel_buf
        (gx, gy, gz) <- gyro_get_sample gyro_buf
        call_ kalman_predict dt_seconds gx gy gz ax ay az
        call_ kalman_output
        store timestamp_ref now
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
      (ax, ay, az) <- accel_get_sample accel_buf
      (mx, my, mz) <- mag_get_sample mag_buf
      p <- baro_get_sample baro_buf
      call_ kalman_init ax ay az mx my mz p
    when (i ==? 8) ow

  accel_get_sample buf = do
    ax <- deref ((buf ~> A.sample) ~> XYZ.x)
    ay <- deref ((buf ~> A.sample) ~> XYZ.y)
    az <- deref ((buf ~> A.sample) ~> XYZ.z)
    return (ax, ay, az)

  gyro_get_sample buf = do
    let toRads deg = deg * pi / 180.0
    gx <- fmap toRads $ deref ((buf ~> G.sample) ~> XYZ.x)
    gy <- fmap toRads $ deref ((buf ~> G.sample) ~> XYZ.y)
    gz <- fmap toRads $ deref ((buf ~> G.sample) ~> XYZ.z)
    return (gx, gy, gz)


  mag_get_sample buf = do
    let toMilligauss gauss = gauss * 1000
    mx <- fmap toMilligauss $ deref ((buf ~> M.sample) ~> XYZ.x)
    my <- fmap toMilligauss $ deref ((buf ~> M.sample) ~> XYZ.y)
    mz <- fmap toMilligauss $ deref ((buf ~> M.sample) ~> XYZ.z)
    return (mx, my, mz)

  baro_get_sample buf = do
    let toPascals mbar = mbar * 100
    fmap toPascals $ deref (buf ~> B.pressure)

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

  kalman_init :: Def ('[IFloat, IFloat, IFloat, IFloat, IFloat, IFloat, IFloat] :-> ())
  kalman_init = proc "kalman_init" $ \ accX accY accZ magX magY magZ pressure -> body $ do
    let acc = xyz accX accY accZ
    let mag = xyz magX magY magZ
    kalmanInit (addrOf kalman_state) (addrOf kalman_covariance) acc mag pressure

  kalman_predict :: Def ('[IFloat, IFloat, IFloat, IFloat, IFloat, IFloat, IFloat] :-> ())
  kalman_predict = proc "kalman_predict" $ \ dt dax day daz dvx dvy dvz -> body $ do
    let distVector = DisturbanceVector { disturbanceGyro = xyz dax day daz -- delta angle
                                       , disturbanceAccel = xyz dvx dvy dvz } -- delta velocity
    kalmanPredict (addrOf kalman_state) (addrOf kalman_covariance) dt distVector

  kalman_output :: Def ('[]:->())
  kalman_output = proc "kalman_output" $ body $ do
    accel_get_sample accel_buf >>= print_three_floats
    gyro_get_sample  gyro_buf  >>= print_three_floats
    mag_get_sample   mag_buf   >>= print_three_floats
    baro_get_sample  baro_buf  >>= print_float

    print_array (addrOf kalman_state ~> orient)
    print_array (addrOf kalman_state ~> vel)
    print_array (addrOf kalman_state ~> pos)
    endl
    where
    print_array a = arrayMap $ \ix ->
      deref (a ! ix) >>= print_float
    print_three_floats (x,y,z) = do
      print_float x
      print_float y
      print_float z


    print_float :: IFloat -> Ivory eff ()
    print_float v = call_ printf_float "%f " v

    endl :: Ivory eff ()
    endl = call_ puts ""

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
    , "CFLAGS = -Wall -Os -std=c99 -D_BSD_SOURCE -Wno-unused-variable -I."
    , "LDLIBS = -lm"
    , "OBJS = " ++ intercalate " " objects
    , exename ++ ": $(OBJS)"
    , "clean:"
    , "\t-rm -f $(OBJS) " ++ exename
    , ".PHONY: clean"
    ]
