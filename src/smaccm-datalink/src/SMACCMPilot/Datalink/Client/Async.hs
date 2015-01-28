
module SMACCMPilot.Datalink.Client.Async
  ( asyncRun
  , asyncRunGW
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

asyncRunGW :: Console -> String -> GW () -> IO (A.Async ())
asyncRunGW console name act = asyncRun name $ runGW (annotate console name) act

asyncRunEffect :: Console -> String -> Effect GW () -> IO (A.Async ())
asyncRunEffect console name eff = asyncRunGW console name (runEffect eff)
