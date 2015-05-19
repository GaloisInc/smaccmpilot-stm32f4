{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Char (ord)
import Ivory.Artifact
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)
import SMACCMPilot.Datalink.HXStream.Ivory
import Ivory.Language
import Ivory.Serialize
import Ivory.Stdlib
import SMACCMPilot.Hardware.GPS.Types
import SMACCMPilot.Comm.Ivory.Types (typeModules)
import SMACCMPilot.Comm.Ivory.Types.AccelerometerSample
import SMACCMPilot.Comm.Ivory.Types.GyroscopeSample
import SMACCMPilot.Comm.Ivory.Types.BarometerSample
import SMACCMPilot.Comm.Ivory.Types.MagnetometerSample

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

baro :: Def ('[ConstRef s (Struct "barometer_sample")] :-> ())
baro = proc "baro" $ \ _ -> body $ do
  call_ puts "baro"

mag :: Def ('[ConstRef s (Struct "magnetometer_sample")] :-> ())
mag = proc "mag" $ \ _ -> body $ do
  call_ puts "mag"

gyro :: Def ('[ConstRef s (Struct "gyroscope_sample")] :-> ())
gyro = proc "gyro" $ \ _ -> body $ do
  call_ puts "gyro"

accel :: Def ('[ConstRef s (Struct "accelerometer_sample")] :-> ())
accel = proc "accel" $ \ _ -> body $ do
  call_ puts "accel"

gps :: Def ('[ConstRef s (Struct "position")] :-> ())
gps = proc "gps" $ \ _ -> body $ do
  call_ puts "gps"

getchar :: Def ('[] :-> Sint32)
getchar = importProc "getchar" "stdio.h"

puts :: Def('[IString]:-> Sint32)
puts = importProc "puts" "stdio.h"


decoder :: Def ('[] :-> Sint32)
decoder = proc "main" $ body $ do
  hxstate <- local initStreamState
  buf <- local (izero :: Init (Array 256 (Stored Uint8)))
  forever $ do
    nextchar <- call getchar
    when (nextchar <? 0) breakOut
    noReturn $ noBreak $ decodes
      [ handler buf 'b' baro
      , handler buf 'm' mag
      , handler buf 'g' gyro
      , handler buf 'a' accel
      , handler buf 'p' gps
      ]
      hxstate
      (castDefault nextchar)
  ret 0

decoderModule :: Module
decoderModule = package "decoder" $ do
  depend gpsTypesModule
  depend accelerometerSampleTypesModule
  depend gyroscopeSampleTypesModule
  depend magnetometerSampleTypesModule
  depend barometerSampleTypesModule
  depend serializeModule
  depend hxstreamModule
  incl baro
  incl mag
  incl gyro
  incl accel
  incl gps
  incl getchar
  incl puts
  incl decoder

main :: IO ()
main = C.compile modules artifacts
  where
  modules = [decoderModule, gpsTypesModule, serializeModule, hxstreamModule]
         ++ typeModules
  artifacts = makefile : serializeArtifacts
  makefile = Root $ artifactString "Makefile" $ unlines [
      "CC = gcc",
      "CFLAGS = -Wall -O0 -g -I. -DIVORY_TEST",
      "LDLIBS = -lm",
      "OBJS = " ++ intercalate " " [ moduleName m ++ ".o" | m <- modules ],
      "decoder: $(OBJS)",
      "clean:",
      "\t-rm -f $(OBJS)",
      ".PHONY: clean"
    ]
