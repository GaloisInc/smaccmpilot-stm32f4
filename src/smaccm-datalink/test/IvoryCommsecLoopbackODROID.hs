
module Main where

import Ivory.Language
import Ivory.Language.Proxy
import Ivory.Stdlib
import Ivory.Tower
import Tower.AADL
import Ivory.Tower.Config

import           Tower.Odroid.UART
import qualified Ivory.Tower.HAL.Bus.Interface as I

import SMACCMPilot.Commsec.SymmetricKey

import SMACCMPilot.Datalink.Loopback

--------------------------------------------------------------------------------

main :: IO ()
main = compileTowerAADL fst p (app snd)
  where
  p topts = getConfig topts $ do
    c <- aadlConfigParser defaultAADLConfig
    k <- symmetricKeyParser
    return (c,k)


app :: (e -> SymmetricKey)
    -> Tower e ()
app tosk = do
  towerModule  stdlibStringModule
  towerDepends stdlibStringModule
  _ <- mapM towerArtifact stdlibStringArtifacts

  (o, i)    <- uartTower
  valueChan <- channel
  bytesChan <- channel

  monitor "uartTyMon" $ do
    packet <- stateInit "uartPacket" (izero :: Init UartPacket)

    -- Handles commsec input, emitting to driver
    handler (snd valueChan) "uartTyHan" $ do
      e <- emitter (I.backpressureTransmit o) 1
      callback $ \msg -> do
        let arr = packet ~> stringDataL
        sz_from_istr arr msg
        emit e (constRef packet)

  let o' = o { I.backpressureTransmit = fst valueChan }

  monitor "to_hx" $ do
    handler i "unpack" $ do
      let n = fromTypeNat (aNat :: Proxy (Capacity UartPacket))
      e <- emitter (fst bytesChan) n
      callback $ \msg -> do
        len <- msg ~>* stringLengthL
        let d = msg ~> stringDataL
        arrayMap $ \ix -> do
          when (fromIx ix <? len) $ emit e (d!ix)

  sk <- fmap tosk getEnv
  frame_loopback sk o' (snd bytesChan)

