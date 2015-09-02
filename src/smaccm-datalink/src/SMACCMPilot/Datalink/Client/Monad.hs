{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SMACCMPilot.Datalink.Client.Monad
  ( DLIO
  , runDLIO
  , MonadIO(..)
  , writeErr
  , writeLog
  , writeDbg
  ) where

import MonadLib

import Control.Applicative
import Control.Monad.IO.Class

import SMACCMPilot.Datalink.Client.Console

--------------------------------------------------------------------------------

newtype DLIO a =
  DLIO { unDLIO :: ReaderT Console IO a }
  deriving (Functor, Monad, Applicative)

instance MonadIO DLIO where
  liftIO a = DLIO (lift a)

runDLIO :: Console ->  DLIO a -> IO a
runDLIO console gw = runReaderT console (unDLIO gw)

getConsole :: DLIO Console
getConsole = DLIO ask

writeErr :: String -> DLIO ()
writeErr msg = do
  c <- getConsole
  liftIO $ consoleError c msg

writeDbg :: String -> DLIO ()
writeDbg msg = do
  c <- getConsole
  liftIO $ consoleDebug c msg

writeLog :: String -> DLIO ()
writeLog msg = do
  c <- getConsole
  liftIO $ consoleLog c msg

