{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

import Data.List
import Ivory.Artifact
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)
import Ivory.HXStream
import Ivory.Language
import Ivory.Serialize
import Ivory.Stdlib
import SMACCMPilot.Hardware.GPS.Types
import SMACCMPilot.Hardware.HMC5883L.Types
import SMACCMPilot.Hardware.MPU6000.Types
import SMACCMPilot.Hardware.MS5611.Types

handler :: (ANat len, IvoryArea a, IvoryZero a, IvorySizeOf a, SerializableRef a)
        => Ref s1 (Array len (Stored Uint8))
        -> Tag
        -> (forall s2. Def ('[ConstRef s2 a] :-> ()))
        -> FrameHandler
handler buf tag consumer = mkFrameHandler ScopedFrameHandler
  { fhTag = tag
  , fhBegin = return ()
  , fhData = \ b off -> do
      assert $ off <? arrayLen buf
      store (buf ! toIx off) b
  , fhEnd = do
      result <- local izero
      unpackFrom_ (constRef buf) 0 $ munpack result
      call_ consumer $ constRef result
  }

baro :: Def ('[ConstRef s (Struct "ms5611_measurement")] :-> ())
baro = proc "baro" $ \ _ -> body $ return ()

compass :: Def ('[ConstRef s (Struct "hmc5883l_sample")] :-> ())
compass = proc "compass" $ \ _ -> body $ return ()

gyro :: Def ('[ConstRef s (Struct "mpu6000_sample")] :-> ())
gyro = proc "gyro" $ \ _ -> body $ return ()

gps :: Def ('[ConstRef s (Struct "position")] :-> ())
gps = proc "gps" $ \ _ -> body $ return ()

getchar :: Def ('[] :-> Sint32)
getchar = importProc "getchar" "stdio.h"

decoder :: Def ('[] :-> Sint32)
decoder = proc "main" $ body $ do
  hxstate <- local initStreamState
  buf <- local (izero :: Init (Array 46 (Stored Uint8)))
  forever $ do
    nextchar <- call getchar
    when (nextchar <? 0) breakOut
    noReturn $ noBreak $ decodes [ handler buf 98 baro, handler buf 99 compass, handler buf 103 gyro, handler buf 112 gps ] hxstate $ castDefault nextchar
  ret 0

decoderModule :: Module
decoderModule = package "decoder" $ do
  depend gpsTypesModule
  depend hmc5883lTypesModule
  depend mpu6000TypesModule
  depend ms5611TypesModule
  depend serializeModule
  depend hxstreamModule
  incl baro
  incl compass
  incl gyro
  incl gps
  incl getchar
  incl decoder

main :: IO ()
main = C.compile modules artifacts
  where
  modules = [decoderModule, gpsTypesModule, hmc5883lTypesModule, mpu6000TypesModule, ms5611TypesModule, serializeModule, hxstreamModule]
  artifacts = makefile : serializeArtifacts
  makefile = artifactString "Makefile" $ unlines [
      "CC = gcc",
      "CFLAGS = -Wall -Og -g -I. -DIVORY_TEST",
      "LDLIBS = -lm",
      "OBJS = " ++ intercalate " " [ moduleName m ++ ".o" | m <- modules ],
      "decoder: $(OBJS)",
      "clean:",
      "\t-rm -f $(OBJS)",
      ".PHONY: clean"
    ]
