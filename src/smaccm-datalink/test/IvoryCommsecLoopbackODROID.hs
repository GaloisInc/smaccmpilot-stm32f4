
module Main where

import Ivory.Language
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

  (o, i)    <- uartTower
  valueChan <- channel

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

  sk <- fmap tosk getEnv
  frame_loopback sk o' i

