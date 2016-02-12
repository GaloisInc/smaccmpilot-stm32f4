{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SMACCMPilot.Datalink.Client.Monad
  ( GW
  , runGW
  , MonadIO(..)
  , writeErr
  , writeLog
  , writeDbg
  ) where

import Prelude ()
import Prelude.Compat

import MonadLib

import Control.Monad.IO.Class

import SMACCMPilot.Datalink.Client.Console

--------------------------------------------------------------------------------

newtype GW a =
  GW { unGW :: ReaderT Console IO a }
  deriving (Functor, Monad, Applicative)

instance MonadIO GW where
  liftIO a = GW (lift a)

runGW :: Console ->  GW a -> IO a
runGW console gw = runReaderT console (unGW gw)

getConsole :: GW Console
getConsole = GW ask

writeErr :: String -> GW ()
writeErr msg = do
  c <- getConsole
  liftIO $ consoleError c msg

writeDbg :: String -> GW ()
writeDbg msg = do
  c <- getConsole
  liftIO $ consoleDebug c msg

writeLog :: String -> GW ()
writeLog msg = do
  c <- getConsole
  liftIO $ consoleLog c msg

