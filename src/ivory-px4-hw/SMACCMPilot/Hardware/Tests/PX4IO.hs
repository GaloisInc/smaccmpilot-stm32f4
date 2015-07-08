{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Hardware.Tests.PX4IO (app) where

import Ivory.Tower
import SMACCMPilot.Hardware.Tests.Platforms
import SMACCMPilot.Hardware.PX4IO

app :: (e -> PX4Platform)
    -> Tower e ()
app topx4 = do
  e <- getEnv
  px4io_state <- case px4platform_px4io (topx4 e) of
    PX4IO_Serial dmauart pins -> px4ioTower tocc dmauart pins
    PX4IO_None -> error "Cannot build PX4IO Test: not supported on this platform"

  monitor "stub" $ do
    handler px4io_state "new_px4io_state" $
      callback $ const $ return () -- I need a place to put the breakpoint...
  where
  tocc = px4platform_clockconfig . topx4

