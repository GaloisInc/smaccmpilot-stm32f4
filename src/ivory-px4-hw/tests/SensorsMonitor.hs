{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Char (ord)
import Ivory.Language
import Ivory.Serialize
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.OS.Posix.Tower
import Ivory.OS.Posix.Tower.Serial
import SMACCMPilot.Comm.Ivory.Types
import SMACCMPilot.Datalink.HXStream.Tower
import SMACCMPilot.Hardware.GPS.Types

[ivory| string struct Result 8 |]

app :: Tower e ()
app = do
  let resultModule = package "result" $ defStringType (Proxy :: Proxy Result)
  let dependencies =
        [ gpsTypesModule
        , resultModule
        ] ++ typeModules

  mapM_ towerModule dependencies
  mapM_ towerDepends dependencies

  (tx, rx) <- serialIO

  (hxs, hs) <- fmap unzip $ sequence
    [ decl tx 'b' "baro" (packRep :: PackRep (Struct "barometer_sample"))
    , decl tx 'm' "mag" (packRep :: PackRep (Struct "magnetometer_sample"))
    , decl tx 'g' "gyro" (packRep :: PackRep (Struct "gyroscope_sample"))
    , decl tx 'a' "accel" (packRep :: PackRep (Struct "accelerometer_sample"))
    , decl tx 'p' "gps" (packRep :: PackRep (Struct "position"))
    ]

  hxstreamDecodeTower "decoder" rx (Proxy :: Proxy 256) hxs

  monitor "decoder" $ sequence_ hs

decl :: (IvoryArea msg, IvoryZero msg)
     => BackpressureTransmit Result (Stored IBool)
     -> Char
     -> String
     -> PackRep msg
     -> Tower e (HXStreamHandler, Monitor e ())
decl tx tag label rep = do
  (to, from) <- channel
  return $ (,) (hxstreamHandler' (fromIntegral (ord tag)) rep to) $ do
    labelString <- fmap constRef $ stateInit label $ stringInit label
    handler from label $ do
      e <- emitter (backpressureTransmit tx) 1
      callback $ const $ do
        emit e labelString

main :: IO ()
main = compileTowerPosix (const $ return ()) app
