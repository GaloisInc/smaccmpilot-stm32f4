{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.Tests.Serialize
  ( gyroSender
  , accelSender
  , magSender
  , baroSender
  , positionSender
  , sampleSender
  , Sender
  , serializeTowerDeps
  , rateDivider
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import Data.Char (ord)

import Ivory.Tower.HAL.Bus.Interface
import SMACCMPilot.Comm.Ivory.Types.AccelerometerSample ()
import SMACCMPilot.Comm.Ivory.Types.GyroscopeSample ()
import SMACCMPilot.Comm.Ivory.Types.MagnetometerSample ()
import SMACCMPilot.Comm.Ivory.Types.BarometerSample ()
import SMACCMPilot.Comm.Ivory.Types (typeModules)
import SMACCMPilot.Hardware.GPS.Types ()
import SMACCMPilot.Hardware.Tests.Platforms

import qualified SMACCMPilot.Datalink.HXStream.Ivory as HX
import Ivory.Serialize

serializeTowerDeps :: Tower e ()
serializeTowerDeps = do
  mapM_ towerDepends typeModules
  mapM_ towerModule typeModules
  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts

rateDivider :: (IvoryArea a, IvoryZero a)
         => Integer
         -> ChanOutput a
         -> Tower e (ChanOutput a)
rateDivider r c = do
  c' <- channel
  monitor "halfRate" $ do
    st <- stateInit "s" (ival (0 :: Uint32))
    handler c "halfRate" $ do
      e <- emitter (fst c') 1
      callback $ \v -> do
        s <- deref st
        ifte_ (s >=? fromIntegral (r - 1))
          (emit e v >> store st 0)
          (store st (s + 1))
  return (snd c')

type Sender e a = ChanOutput a -> BackpressureTransmit ConsoleBuffer (Stored IBool) -> Monitor e ()

sampleSender :: (ANat len, IvoryArea a, IvoryZero a, Packable a)
             => Char
             -> Proxy len
             -> Sender e a
sampleSender tag len c out = do
  buf <- stateInit "buf" $ izerolen len
  handler c "sender" $ do
    e <- emitter (backpressureTransmit out) 1
    callback $ \ sample -> do
      packInto buf 0 sample
      str <- local izero
      HX.encodeString (fromIntegral $ ord tag) (constRef buf) str
      emit e $ constRef str

gyroSender :: Sender e (Struct "gyroscope_sample")
gyroSender = sampleSender 'g' (Proxy :: Proxy 27)

accelSender :: Sender e (Struct "accelerometer_sample")
accelSender = sampleSender 'a' (Proxy :: Proxy 26)

magSender :: Sender e (Struct "magnetometer_sample")
magSender = sampleSender 'm' (Proxy :: Proxy 23)

baroSender :: Sender e (Struct "barometer_sample")
baroSender = sampleSender 'b' (Proxy :: Proxy 18)

positionSender :: Sender e (Struct "position")
positionSender = sampleSender 'p' (Proxy :: Proxy 46)
