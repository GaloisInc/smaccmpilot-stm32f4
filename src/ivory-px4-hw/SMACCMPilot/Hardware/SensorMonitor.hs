{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

data SensorHandlers =
  SensorHandlers
    { sh_baro  :: forall s s' . ConstRef s (Struct "barometer_sample") -> Ivory (AllocEffects s') ()
    , sh_mag   :: forall s s' . ConstRef s (Struct "magnetometer_sample") -> Ivory (AllocEffects s') ()
    , sh_gyro  :: forall s s' . ConstRef s (Struct "gyroscope_sample") -> Ivory (AllocEffects s') ()
    , sh_accel :: forall s s' . ConstRef s (Struct "accelerometer_sample") -> Ivory (AllocEffects s') ()
    , sh_gps   :: forall s s' . ConstRef s (Struct "position") -> Ivory (AllocEffects s') ()
    , sh_moddef :: ModuleDef
    }

decoder :: SensorHandlers -> ([Module], [Located Artifact])
decoder SensorHandlers{..} = (decoderModule : dependencies, artifacts)
  where
  decoder_proc :: Def ('[] :-> Sint32)
  decoder_proc = proc "main" $ body $ do
    termset <- local izero
    call_ tcgetattr stdinFd termset
    call_ cfmakeraw termset
    call_ cfsetispeed termset b115200
    call_ cfsetospeed termset b115200
    call_ tcsetattr stdinFd tcsaNow $ constRef termset

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
      incl baro_proc
      incl mag_proc
      incl gyro_proc
      incl accel_proc
      incl gps_proc
      sh_moddef

      incl cfmakeraw
      incl cfsetispeed
      incl cfsetospeed
      incl tcsetattr
      incl tcgetattr
      incl getchar

      inclSym b115200
      inclSym tcsaNow
      inclSym stdinFd

  dependencies =
    [ gpsTypesModule
    , serializeModule
    , hxstreamModule
    ] ++ typeModules

  artifacts = serializeArtifacts

  baro_proc :: Def ('[ConstRef s (Struct "barometer_sample")] :-> ())
  baro_proc = proc "baro_sample_handler" $ \ v -> body $ do
    noReturn $ sh_baro v

  mag_proc :: Def ('[ConstRef s (Struct "magnetometer_sample")] :-> ())
  mag_proc = proc "mag_sample_handler" $ \ v -> body $ do
    noReturn $ sh_mag v

  gyro_proc :: Def ('[ConstRef s (Struct "gyroscope_sample")] :-> ())
  gyro_proc = proc "gyro_sample_handler" $ \ v -> body $ do
    noReturn $ sh_gyro v

  accel_proc :: Def ('[ConstRef s (Struct "accelerometer_sample")] :-> ())
  accel_proc = proc "accel_sample_handler" $ \ v -> body $ do
    noReturn $ sh_accel v

  gps_proc :: Def ('[ConstRef s (Struct "position")] :-> ())
  gps_proc = proc "gps_sample_handler" $ \ v -> body $ do
    noReturn $ sh_gps v

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


[ivory| abstract struct termios "termios.h" |]

cfmakeraw :: Def ('[Ref s (Struct "termios")] :-> ())
cfmakeraw = importProc "cfmakeraw" "termios.h"

type BaudRate = Uint32

cfsetispeed :: Def ('[Ref s (Struct "termios"), BaudRate] :-> ())
cfsetispeed = importProc "cfsetispeed" "termios.h"

cfsetospeed :: Def ('[Ref s (Struct "termios"), BaudRate] :-> ())
cfsetospeed = importProc "cfsetospeed" "termios.h"

b115200 :: BaudRate
b115200 = extern "B115200" "termios.h"

tcsaNow :: Sint32
tcsaNow = extern "TCSANOW" "termios.h"

type Fd = Sint32

tcgetattr :: Def ('[Fd, Ref s (Struct "termios")] :-> ())
tcgetattr = importProc "tcgetattr" "termios.h"

tcsetattr :: Def ('[Fd, Sint32, ConstRef s (Struct "termios")] :-> ())
tcsetattr = importProc "tcsetattr" "termios.h"

stdinFd :: Sint32
stdinFd = extern "STDIN_FILENO" "unistd.h"

getchar :: Def ('[] :-> Sint32)
getchar = importProc "getchar" "stdio.h"
