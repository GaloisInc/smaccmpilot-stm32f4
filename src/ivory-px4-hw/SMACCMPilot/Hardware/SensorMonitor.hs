{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module SMACCMPilot.Hardware.SensorMonitor
  ( SensorHandlers(..)
  , decoder
  ) where

import Data.Char (ord)
import SMACCMPilot.Datalink.HXStream.Ivory
import Ivory.Language
import Ivory.Artifact
import Ivory.Serialize
import Ivory.Stdlib
import SMACCMPilot.Hardware.GPS.Types
import SMACCMPilot.Comm.Ivory.Types

import Paths_ivory_px4_hw

data SensorHandlers =
  SensorHandlers
    { sh_baro  :: forall s eff . ConstRef s (Struct "barometer_sample") -> Ivory eff ()
    , sh_mag   :: forall s eff . ConstRef s (Struct "magnetometer_sample") -> Ivory eff ()
    , sh_gyro  :: forall s eff . ConstRef s (Struct "gyroscope_sample") -> Ivory eff ()
    , sh_accel :: forall s eff . ConstRef s (Struct "accelerometer_sample") -> Ivory eff ()
    , sh_gps   :: forall s eff . ConstRef s (Struct "position") -> Ivory eff ()
    , sh_moddef :: ModuleDef
    }

decoder :: SensorHandlers -> ([Module], [Located Artifact])
decoder SensorHandlers{..} = (decoderModule : dependencies, artifacts)
  where
  decoder_proc :: Def ('[] :-> Sint32)
  decoder_proc = proc "main" $ body $ do
    call_ termios_helper_setraw 0 115200
    hxstate <- local initStreamState
    buf <- local (izero :: Init (Array 256 (Stored Uint8)))
    forever $ do
      nextchar <- call getchar
      when (nextchar <? 0) breakOut
      noReturn $ noBreak $ decodes
        [ handler buf 'b' baro_proc
        , handler buf 'm' mag_proc
        , handler buf 'g' gyro_proc
        , handler buf 'a' accel_proc
        , handler buf 'p' gps_proc
        ]
        hxstate
        (castDefault nextchar)
    ret 0

  decoderModule :: Module
  decoderModule = package "decoder" $ do
    mapM_ depend dependencies
    incl decoder_proc
    private $ do
      incl termios_helper_setraw
      incl baro_proc
      incl mag_proc
      incl gyro_proc
      incl accel_proc
      incl gps_proc
      incl getchar
      sh_moddef

  dependencies =
    [ gpsTypesModule
    , serializeModule
    , hxstreamModule
    ] ++ typeModules

  baro_proc :: Def ('[ConstRef s (Struct "barometer_sample")] :-> ())
  baro_proc = proc "baro_sample_handler" $ \ v -> body $ do
    sh_baro v

  mag_proc :: Def ('[ConstRef s (Struct "magnetometer_sample")] :-> ())
  mag_proc = proc "mag_sample_handler" $ \ v -> body $ do
    sh_mag v

  gyro_proc :: Def ('[ConstRef s (Struct "gyroscope_sample")] :-> ())
  gyro_proc = proc "gyro_sample_handler" $ \ v -> body $ do
    sh_gyro v

  accel_proc :: Def ('[ConstRef s (Struct "accelerometer_sample")] :-> ())
  accel_proc = proc "accel_sample_handler" $ \ v -> body $ do
    sh_accel v

  gps_proc :: Def ('[ConstRef s (Struct "position")] :-> ())
  gps_proc = proc "gps_sample_handler" $ \ v -> body $ do
    sh_gps v

  termios_helper_setraw :: Def ('[Sint32, Sint32] :-> ())
  termios_helper_setraw = importProc "termios_helper_setraw" "termios_helpers.h"

  cabalArtifact f = artifactCabalFile Paths_ivory_px4_hw.getDataDir ("support/" ++ f)

  artifacts = [ Incl $ cabalArtifact "termios_helpers.h"
              , Src  $ cabalArtifact "termios_helpers.c"
              ] ++ serializeArtifacts

handler :: (ANat len, IvoryArea a, IvoryZero a, Packable a)
        => Ref s1 (Array len (Stored Uint8))
        -> Char
        -> (forall s2. Def ('[ConstRef s2 a] :-> ()))
        -> FrameHandler
handler buf tag consumer = mkFrameHandler ScopedFrameHandler
  { fhTag = fromIntegral $ ord tag
  , fhBegin = return ()
  , fhData = \ b off -> do
      assert $ off <? arrayLen buf
      store (buf ! toIx off) b
  , fhEnd = do
      result <- local izero
      unpackFrom (constRef buf) 0 result
      call_ consumer $ constRef result
  }


getchar :: Def ('[] :-> Sint32)
getchar = importProc "getchar" "stdio.h"
