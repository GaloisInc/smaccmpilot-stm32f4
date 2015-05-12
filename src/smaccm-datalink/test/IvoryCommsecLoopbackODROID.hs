{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.Config
import Ivory.Tower.Compile.Options (TOpts(..))
import Tower.AADL

import           Tower.Odroid.UART
import qualified Ivory.Tower.HAL.Bus.Interface as I

import SMACCMPilot.Commsec.SymmetricKey
import SMACCMPilot.Commsec.Ivory.Artifacts

import SMACCMPilot.Datalink.Loopback

import System.Environment

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  opts <- parseOpts args
  key  <- getConfig topts symmetricKeyParser
  runCompileAADL opts c (app key)
  where
  c = addAadlArtifacts commsecArtifacts uartConfig
  topts = TOpts Nothing False [] error

app :: SymmetricKey
    -> Tower e ()
app key = do

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

--  sk <- fmap tosk getEnv
  frame_loopback key o' i

