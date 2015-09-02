
module SMACCMPilot.Datalink.Client.Async
  ( asyncRun
  , asyncRunDLIO
  , asyncRunEffect
  , A.wait
  ) where

import           Control.Exception
import qualified Control.Concurrent.Async        as A
import           System.IO
import           Pipes

import SMACCMPilot.Datalink.Client.Console
import SMACCMPilot.Datalink.Client.Monad

asyncRun :: String -> IO () -> IO (A.Async ())
asyncRun name act = A.async $ catch act exit
  where
  exit :: SomeException -> IO ()
  exit x = hPutStrLn stderr $ "asyncRun " ++ name ++ " exception: " ++ (show x)

asyncRunDLIO :: Console -> String -> DLIO () -> IO (A.Async ())
asyncRunDLIO console name act = asyncRun name $ runDLIO (annotate console name) act

asyncRunEffect :: Console -> String -> Effect DLIO () -> IO (A.Async ())
asyncRunEffect console name eff = asyncRunDLIO console name (runEffect eff)
