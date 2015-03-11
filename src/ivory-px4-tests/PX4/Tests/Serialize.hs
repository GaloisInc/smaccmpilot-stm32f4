{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module PX4.Tests.Serialize
  ( gyroSender
  , accelSender
  , magSender
  , baroSender
  , positionSender
  , serializeTowerDeps
  , halfRate
  ) where

import Ivory.Language
import Ivory.Tower

import Data.Char (ord)

import SMACCMPilot.Hardware.Types.Accelerometer ()
import SMACCMPilot.Hardware.Types.Gyroscope ()
import SMACCMPilot.Hardware.Types.Magnetometer ()
import SMACCMPilot.Hardware.Types.Barometer ()
import SMACCMPilot.Hardware.GPS.Types ()

import qualified SMACCMPilot.Datalink.HXStream.Ivory as HX
import Ivory.Serialize

serializeTowerDeps :: Tower e ()
serializeTowerDeps = do
  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts

halfRate :: (IvoryArea a, IvoryZero a)
         => ChanOutput a
         -> Tower e (ChanOutput a)
halfRate c = do
  c' <- channel
  monitor "halfRate" $ do
    st <- state "s"
    handler c "halfRate" $ do
      e <- emitter (fst c') 1
      callback $ \v -> do
        s <- deref st
        ifte_ s
          (emit e v >> store st false)
          (store st true)
  return (snd c')

gyroSender :: ChanOutput (Struct "gyroscope_sample")
           -> ChanInput (Stored Uint8)
           -> Monitor e ()
gyroSender c out = do
  (buf :: Ref Global (Array 26 (Stored Uint8))) <- state "gyro"
  handler c "gyro_sender" $ do
    e <- emitter out (26*2 + 3)
    callback $ \s -> noReturn $ do
      packInto buf 0 s
      HX.encode tag (constRef buf) (emitV e)
  where
  tag = tagChar 'g'

accelSender :: ChanOutput (Struct "accelerometer_sample")
            -> ChanInput (Stored Uint8)
            -> Monitor e ()
accelSender c out = do
  (buf :: Ref Global (Array 26 (Stored Uint8))) <- state "accel"
  handler c "accel_sender" $ do
    e <- emitter out (26*2 + 3)
    callback $ \s -> noReturn $ do
      packInto buf 0 s
      HX.encode tag (constRef buf) (emitV e)
  where
  tag = tagChar 'a'

magSender :: ChanOutput (Struct "magnetometer_sample")
            -> ChanInput (Stored Uint8)
            -> Monitor e ()
magSender c out = do
  (buf :: Ref Global (Array 22 (Stored Uint8))) <- state "mag"
  handler c "mag_sender" $ do
    e <- emitter out (22*2 + 3)
    callback $ \s -> noReturn $ do
      packInto buf 0 s
      HX.encode tag (constRef buf) (emitV e)
  where
  tag = tagChar 'm'

baroSender :: ChanOutput (Struct "barometer_sample")
            -> ChanInput (Stored Uint8)
            -> Monitor e ()
baroSender c out = do
  (buf :: Ref Global (Array 18 (Stored Uint8))) <- state "baro"
  handler c "baro_sender" $ do
    e <- emitter out (18*2 + 3)
    callback $ \s -> noReturn $ do
      packInto buf 0 s
      HX.encode tag (constRef buf) (emitV e)
  where
  tag = tagChar 'b'

positionSender :: ChanOutput (Struct "position")
               -> ChanInput (Stored Uint8)
               -> Monitor p ()
positionSender pos out = do
  (buf :: Ref Global (Array 46 (Stored Uint8))) <- state "pos_ser_buf"
  handler pos "position_serialize" $ do
    e <- emitter out (46*2+3)
    callback $ \p -> noReturn $ do
      packInto buf 0 p
      HX.encode tag (constRef buf) (emitV e)
  where
  tag = tagChar 'p'



tagChar :: Char -> Uint8
tagChar = fromIntegral . ord
