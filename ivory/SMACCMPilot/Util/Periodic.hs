{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Util.Periodic
  ( periodic
  ) where

import Ivory.Language
import qualified Ivory.OS.FreeRTOS.Task as Task

periodic :: (eff `AllocsIn` s) => Integer -> Ivory eff () -> Ivory eff ()
periodic period f = do
  initTime <- call Task.getTimeMillis
  lastTime <- local (ival initTime)
  forever $ do
    f
    call_ Task.delayUntil lastTime (fromIntegral period)
