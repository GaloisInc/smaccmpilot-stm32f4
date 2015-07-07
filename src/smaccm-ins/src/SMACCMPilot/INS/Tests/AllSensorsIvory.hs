{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.INS.Tests.AllSensorsIvory
  ( app
  ) where

import Data.Char
import Data.Foldable
import Data.Traversable
import Ivory.Language
import Ivory.Serialize
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.OS.Posix.Tower
import Ivory.OS.Posix.Tower.Serial
import qualified MonadLib
import Numeric.Estimator.Model.Coordinate
import qualified SMACCMPilot.Comm.Ivory.Types.AccelerometerSample as A
import qualified SMACCMPilot.Comm.Ivory.Types.GyroscopeSample     as G
import qualified SMACCMPilot.Comm.Ivory.Types.MagnetometerSample  as M
import qualified SMACCMPilot.Comm.Ivory.Types.TimeMicros          as T
import qualified SMACCMPilot.Comm.Ivory.Types.Xyz                 as XYZ
import qualified SMACCMPilot.Comm.Ivory.Types.XyzCalibration      as C
import SMACCMPilot.Datalink.HXStream.Tower
import SMACCMPilot.INS.Bias.Calibration
import SMACCMPilot.INS.Bias.Gyro
import SMACCMPilot.INS.Bias.Magnetometer
import SMACCMPilot.INS.Ivory
import SMACCMPilot.INS.Tower

import Prelude hiding (mapM, mapM_)

imin :: IvoryOrd t => t -> t -> t
imin a b = (a <? b) ? (a, b)

snprintf_float :: Def('[Ref s (CArray (Stored Uint8)), Uint32, IString, IFloat] :-> Sint32)
snprintf_float = importProc "snprintf" "stdio.h"

snprintf_double :: Def('[Ref s (CArray (Stored Uint8)), Uint32, IString, IDouble] :-> Sint32)
snprintf_double = importProc "snprintf" "stdio.h"

[ivory| string struct Buffer 1024 |]

sprintf_append :: (ANat len, IvoryString str)
               => Ref s' str
               -> Proxy len
               -> (Ref (Stack s) (CArray (Stored Uint8)) -> Uint32 -> Ivory (AllocEffects s) Sint32)
               -> Ivory (AllocEffects s) ()
sprintf_append str bound f = do
  buf <- local (izerolen bound)

  len <- noReturn $ f (toCArray buf) (arrayLen buf)
  comment "Format errors are programmer errors and shouldn't happen."
  assert (len >=? 0)
  comment "Attempting to write past the end of the buffer is a programmer error."
  comment "snprintf appends a '\\0' so we can't use the last element of the buffer either."
  assert (len <? arrayLen buf)

  comment "Now append the formatted buffer to the destination string."
  destLen <- str ~>* stringLengthL
  arrayCopy (str ~> stringDataL) (constRef buf) destLen len
  let maxLen = arrayLen (str ~> stringDataL)
  store (str ~> stringLengthL) (imin (destLen + len) maxLen)

sprintf_float :: Def ('[Ref s Buffer, IString, IFloat] :-> ())
sprintf_float = proc "sprintf_float" $ \ buf fmt arg -> body $ noReturn $
  sprintf_append buf (Proxy :: Proxy 64) (\ tmp len -> call snprintf_float tmp len fmt arg)

sprintf_double :: Def ('[Ref s Buffer, IString, IDouble] :-> ())
sprintf_double = proc "sprintf_double" $ \ buf fmt arg -> body $ noReturn $
  sprintf_append buf (Proxy :: Proxy 64) (\ tmp len -> call snprintf_double tmp len fmt arg)

formattingModule :: Module
formattingModule = package "formatting" $ do
  incl snprintf_float
  incl snprintf_double
  defStringType (Proxy :: Proxy Buffer)
  incl sprintf_float
  incl sprintf_double

fromHX :: (IvoryArea a, IvoryZero a, Packable a)
       => Char
       -> MonadLib.WriterT [HXStreamHandler] (Tower e) (ChanOutput a)
fromHX tag = do
  (to, from) <- MonadLib.lift channel
  MonadLib.put [hxstreamHandler (fromIntegral (ord tag)) to]
  return from

save :: (IvoryArea a, IvoryZero a)
     => String
     -> ChanOutput a
     -> Monitor e (ConstRef Global a)
save name src = do
  copy <- state name
  handler src ("save_" ++ name) $ do
    callback $ refCopy copy
  return $ constRef copy

app :: IO ()
app = compileTowerPosix (const $ return ()) $ do
  towerModule formattingModule
  towerDepends formattingModule

  (tx, rx) <- serialIO

  ((), hxs) <- MonadLib.runWriterT $ do
    accel <- fromHX 'a'
    rawGyro <- fromHX 'g'
    rawMag <- fromHX 'm'
    baro <- fromHX 'b'
    position <- fromHX 'p'

    MonadLib.lift $ do
      (_, controlLaw) <- channel

      currentGyroBias <- calcGyroBiasTower rawGyro accel
      (gyro, gyroBias) <- applyCalibrationTower gyroCalibrate rawGyro currentGyroBias controlLaw

      currentMagBias <- calcMagBiasTower rawMag
      (mag, magBias) <- applyCalibrationTower magCalibrate rawMag currentMagBias controlLaw

      states <- sensorFusion accel gyro mag baro position

      monitor "output" $ do
        lastAccel <- save "accel" accel
        lastGyro <- save "gyro" gyro
        lastGyroBias <- save "gyroBias" gyroBias
        lastMag <- save "mag" mag
        lastMagBias <- save "magBias" magBias

        pending <- state "write_pending"

        let columnnames =
              [ "time"
              -- raw sensor values
              , "ax"
              , "ay"
              , "az"
              , "gx"
              , "gy"
              , "gz"
              , "mx"
              , "my"
              , "mz"
              -- EKF states
              , "q0"
              , "q1"
              , "q2"
              , "q3"
              , "magn"
              , "mage"
              , "magd"
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

        handler states "print_row" $ do
          e <- emitter (backpressureTransmit tx) 1
          callback $ \ kalman_state -> do
            was_pending <- deref pending
            unless was_pending $ do
              buf <- local izero

              timestamp <- lastAccel ~>* A.time
              call_ sprintf_double buf "%12.6f" $ safeCast (T.unTimeMicros timestamp) / 1.0e6

              let print_float :: IFloat -> Ivory eff ()
                  print_float = call_ sprintf_float buf " % f"
              let print_array :: ANat n => ConstRef s (Array n (Stored IFloat)) -> Ivory eff ()
                  print_array a = arrayMap $ \ ix -> noBreak $ deref (a ! ix) >>= print_float

              -- raw sensor values

              accelMSS <- mapM deref $ xyzRefs $ lastAccel ~> A.sample
              mapM_ print_float accelMSS

              gyroDegs <- mapM deref $ xyzRefs $ lastGyro ~> G.sample
              let toRads deg = deg * pi / 180.0
              mapM_ print_float $ fmap toRads gyroDegs

              magGauss <- mapM deref $ xyzRefs $ lastMag ~> M.sample
              let toMilligauss gauss = gauss * 1000
              mapM_ print_float $ fmap toMilligauss magGauss

              -- EKF states

              print_array (kalman_state ~> orient)
              print_array (kalman_state ~> mag_ned)

              -- Non EKF states

              gyroBiases <- mapM deref $ xyzRefs $ lastGyroBias ~> C.bias
              mapM_ print_float gyroBiases
              print_float =<< lastGyroBias ~>* C.progress

              magBiases <- mapM deref $ xyzRefs $ lastMagBias ~> C.bias
              mapM_ print_float magBiases
              print_float =<< lastMagBias ~> C.scale ~>* XYZ.x
              print_float =<< lastMagBias ~>* C.progress

              len <- buf ~>* stringLengthL
              let finalLen = imin (len + 1) (arrayLen (buf ~> stringDataL))
              store (buf ~> stringDataL ! toIx (finalLen - 1)) (fromIntegral (ord '\n'))
              store (buf ~> stringLengthL) finalLen

              emit e $ constRef buf

              store pending true

        handler (backpressureComplete tx) "write_complete" $ do
          callback $ const $ store pending false

        handler systemInit "print_header" $ do
          e <- emitter (backpressureTransmit tx) 1
          callback $ const $ do
            cols <- local $ stringInit $ unwords columnnames ++ "\n"
            emit e $ constRef cols
            store pending true

  hxstreamDecodeTower "decoder" rx (Proxy :: Proxy 256) hxs

  where
  xyzRefs s = fmap (s ~>) $ xyz XYZ.x XYZ.y XYZ.z
