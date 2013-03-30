{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Util.Periodic
  ( periodic
  ) where

import Ivory.Language
import qualified Ivory.OS.FreeRTOS as OS

periodic :: (eff `AllocsIn` s) => Integer -> Ivory eff () -> Ivory eff ()
periodic period f = do
  initTime <- call (direct OS.getTimeMillis)
  lastTime <- local (ival initTime)
  forever $ do
    f
    call (direct_ OS.delayUntil lastTime (fromIntegral period))

