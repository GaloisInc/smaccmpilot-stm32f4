{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module SMACCMPilot.INS.Tests.AllSensorsIvory
  ( app
  ) where

import Control.Lens hiding ((<.>), ix)
import Linear
import Data.Foldable
import Data.List (intercalate)
import Data.String
import Data.Traversable
import System.FilePath

import Ivory.Language
import Ivory.Stdlib
import Ivory.Artifact
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)

import Numeric.Estimator.Model.Coordinate

import SMACCMPilot.INS.Ivory
import SMACCMPilot.INS.SensorFusion
import SMACCMPilot.INS.Bias.Gyro
import SMACCMPilot.INS.Bias.Magnetometer.Diversity
import SMACCMPilot.INS.Bias.Magnetometer.Types (magnetometerBiasTypesModule)
import SMACCMPilot.Hardware.SensorMonitor
import qualified SMACCMPilot.Comm.Ivory.Types.AccelerometerSample as A
import qualified SMACCMPilot.Comm.Ivory.Types.GyroscopeSample     as G
import qualified SMACCMPilot.Comm.Ivory.Types.MagnetometerSample  as M
import qualified SMACCMPilot.Comm.Ivory.Types.Xyz                 as XYZ
import qualified SMACCMPilot.Comm.Ivory.Types.TimeMicros          as T

import Prelude hiding (mapM, mapM_)

printf_float :: Def('[IString, IFloat] :-> ())
printf_float = importProc "printf" "stdio.h"

printf_double :: Def('[IString, IDouble] :-> ())
printf_double = importProc "printf" "stdio.h"

puts :: Def('[IString] :-> ())
puts = importProc "puts" "stdio.h"

linebuf :: Def('[] :-> ())
linebuf = importProc "setlinebuf_stdout" "stdio.h"

imports :: ModuleDef
imports = do
  incl printf_float
  incl printf_double
  incl puts
  incl linebuf


sensorMonitor :: ([Module], [Located Artifact])
sensorMonitor = decoder $ SensorHandlers
  { sh_baro = const $ return ()

  , sh_mag = \mag_sample -> do
      refCopy mag_buf mag_sample
      mag <- mapM deref $ xyzRefs (mag_sample ~> M.sample)
      mag_array <- local $ xyzArr $ fmap (* 1000) mag
      mbe_sample mbe (constRef mag_array)
      mbe_bias <- local izero
      mbe_ready <- mbe_output mbe mbe_bias
      when (mbe_ready >=? 1) $ do
        check_init init_mag $ do
          call_ mag_measure

  , sh_gyro = \gyro_sample -> do
      refCopy gyro_buf gyro_sample
      check_init init_gyro $ do
        now <- deref (gyro_buf ~> G.time)
        prev <- deref timestamp_ref
        let dt_micros :: Sint32
            dt_micros = castDefault $ (T.unTimeMicros now) - (T.unTimeMicros prev)
            dt_seconds = (safeCast dt_micros) / 1000000.0

        call_ kalman_predict dt_seconds
        store timestamp_ref now
        call_ kalman_output

  , sh_accel = \accel_sample -> do
      refCopy accel_buf accel_sample
      check_init 0 $ do
        call_ accel_measure

  , sh_gps = const $ return ()
  , sh_moddef = do
      defMemArea gyro_area
      defMemArea mag_area
      defMemArea accel_area
      defMemArea init_area
      defMemArea timestamp_area
      imports
      ins_moddef
      gbe_moddef
      mbe_moddef
  }

  where
  init_area = area "init_state" Nothing
  init_ref :: Ref Global (Stored Uint32)
  init_ref = addrOf init_area
  init_mag = 1
  init_gyro = 2
  init_done = 4

  check_init :: Uint32 -> Ivory eff () -> Ivory eff ()
  check_init field ow = do
    i <- deref init_ref
    when (i <? 3) $ do
      store init_ref (i .| field)
    when (i ==? 3) $ do
      store init_ref init_done
      call_ kalman_init
      refCopy timestamp_ref (accel_buf ~> A.time)
      call_ puts $ fromString $ unwords columnnames
      call_ linebuf
      call_ kalman_output
    when (i ==? 4) ow

  xyzRefs s = fmap (s ~>) $ xyz XYZ.x XYZ.y XYZ.z

  accel_get_sample :: Ivory eff (XYZ IFloat)
  accel_get_sample = do
    mapM deref $ xyzRefs $ accel_buf ~> A.sample

  gyro_get_sample :: Ivory eff (XYZ IFloat)
  gyro_get_sample = do
    raw <- mapM deref $ xyzRefs $ gyro_buf ~> G.sample
    let toRads deg = deg * pi / 180.0
    return $ fmap toRads raw

  mag_get_sample :: GetAlloc eff ~ Scope s => Ivory eff (XYZ IFloat)
  mag_get_sample = do
    raw <- mapM deref $ xyzRefs $ mag_buf ~> M.sample
    let toMilligauss gauss = gauss * 1000
    mbe_bias <- local izero
    mbe_ready <- mbe_output mbe mbe_bias
    assert mbe_ready
    bias <- mapM deref $ xyz (mbe_bias ! 0) (mbe_bias ! 1) (mbe_bias ! 2)
    return $ fmap toMilligauss $ raw - bias

  timestamp_area = area "timestamp_previous" Nothing
  timestamp_ref = addrOf timestamp_area

  gyro_area = area "gyro_latest" Nothing
  gyro_buf = addrOf gyro_area

  accel_area = area "accel_latest" Nothing
  accel_buf = addrOf accel_area

  mag_area = area "mag_latest" Nothing
  mag_buf = addrOf mag_area


  kalman_state :: MemArea (Struct "kalman_state")
  kalman_state = area "state_vector" Nothing

  kalman_covariance :: MemArea (Struct "kalman_covariance")
  kalman_covariance = area "covariance" Nothing

  kalman_init :: Def ('[] :-> ())
  kalman_init = proc "kalman_init" $ body $ do
    acc <- accel_get_sample
    mag <- mag_get_sample
    kalmanInit (addrOf kalman_state) (addrOf kalman_covariance) acc mag
    gbe_init gbe
    mbe_init mbe

  kalman_predict :: Def ('[IFloat] :-> ())
  kalman_predict = proc "kalman_predict" $ \ dt -> body $ do
    accel <- accel_get_sample
    gyro <- gyro_get_sample
    let distVector = DisturbanceVector { disturbanceGyro = gyro
                                       , disturbanceAccel = accel }
    kalmanPredict (addrOf kalman_state) (addrOf kalman_covariance) dt distVector

    gyro_array <- local (xyzArr gyro)
    gbe_gyro_sample gbe (constRef gyro_array)

  xyzArr :: XYZ IFloat -> Init (Array 3 (Stored IFloat))
  xyzArr (XYZ v) = iarray [ ival (v ^._x), ival (v ^._y), ival (v ^._z) ]

  (gbe, gbe_moddef) = ivoryGyroBiasEstimator "test_gbe"
  (mbe, mbe_moddef) = ivoryDiverseMagBiasEstimator "test_mbe"

  columnnames =
    [ "time"
    , "ax"
    , "ay"
    , "az"
    , "gx"
    , "gy"
    , "gz"
    , "mx"
    , "my"
    , "mz"
    , "q0"
    , "q1"
    , "q2"
    , "q3"
    , "gbiasx"
    , "gbiasy"
    , "gbiasz"
    , "magn"
    , "mage"
    , "magd"
    , "magx"
    , "magy"
    , "magz"
    -- Non EKF states - gyro bias estimator
    , "gbe_x"
    , "gbe_y"
    , "gbe_z"
    , "gbe_good"
    -- Non EKF states - mag bias estimator
    , "mbe_x"
    , "mbe_y"
    , "mbe_z"
    , "mbe_f"
    , "mbe_good"
    ]

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

    -- columns 11-14
    print_array (addrOf kalman_state ~> orient)
    -- columns 15-17
    print_array (addrOf kalman_state ~> gyro_bias)
    -- columns 18-20
    print_array (addrOf kalman_state ~> mag_ned)
    -- columns 21-23
    print_array (addrOf kalman_state ~> mag_xyz)
    -- columns 24-27
    gbe_bias <- local izero
    gbe_good <- gbe_output gbe gbe_bias
    print_gbe gbe_bias gbe_good
    -- columns 28-32
    mbe_bias <- local izero
    mbe_progress <- mbe_output mbe mbe_bias
    print_mbe mbe_bias mbe_progress
    endl
    where
    print_gbe bias good = do
      deref (bias ! 0) >>= print_float
      deref (bias ! 1) >>= print_float
      deref (bias ! 2) >>= print_float
      print_float (good ? (1.0, 0.0))

    print_mbe bias progress = do
      deref (bias ! 0) >>= print_float
      deref (bias ! 1) >>= print_float
      deref (bias ! 2) >>= print_float
      deref (bias ! 3) >>= print_float
      print_float progress

    print_array a = arrayMap $ \ix ->
      deref (a ! ix) >>= print_float

    print_float :: IFloat -> Ivory eff ()
    print_float v = call_ printf_float " % f" v

    endl :: Ivory eff ()
    endl = call_ puts ""

  mag_measure :: Def ('[] :-> ())
  mag_measure = proc "mag_measure" $ body $ do
    mag <- mag_get_sample
    magMeasure (addrOf kalman_state) (addrOf kalman_covariance) mag

  accel_measure :: Def ('[] :-> ())
  accel_measure = proc "accel_measure" $ body $ do
    accel <- accel_get_sample
    accelMeasure (addrOf kalman_state) (addrOf kalman_covariance) accel
    accel_array <- local (xyzArr accel)
    gbe_accel_sample gbe (constRef accel_array)


  ins_moddef :: ModuleDef
  ins_moddef = do
    defStruct (Proxy :: Proxy "kalman_state")
    defStruct (Proxy :: Proxy "kalman_covariance")
    defMemArea kalman_state
    defMemArea kalman_covariance
    incl kalman_init
    incl kalman_predict
    incl kalman_output
    incl mag_measure
    incl accel_measure


app :: IO ()
app = C.compile modules artifacts
  where
  (sens_modules, sens_artifacts) = sensorMonitor
  modules = sens_modules ++ [magnetometerBiasTypesModule, magDiversityHelpersModule]
  artifacts = makefile : sens_artifacts

  objects = [ moduleName m <.> "o" | m <- modules ] ++
    [ replaceExtension (artifactFileName a) "o" | Src a <- sens_artifacts ]

  exename = moduleName $ head modules

  makefile = Root $ artifactString "Makefile" $ unlines
    [ "CC = gcc"
    , "CFLAGS = -Wall -g -Os -std=c99 -D_BSD_SOURCE '-Dsetlinebuf_stdout()=setlinebuf(stdout)' -Wno-unused-variable -I."
    , "LDLIBS = -lm"
    , "OBJS = " ++ intercalate " " objects
    , exename ++ ": $(OBJS)"
    , "clean:"
    , "\t-rm -f $(OBJS) " ++ exename
    , ".PHONY: clean"
    ]
