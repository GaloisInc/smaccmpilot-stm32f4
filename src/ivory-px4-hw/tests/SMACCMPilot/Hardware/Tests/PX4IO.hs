{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Hardware.Tests.PX4IO (app) where

import Ivory.Language
import Ivory.Tower
import SMACCMPilot.Hardware.Platforms
import SMACCMPilot.Hardware.PX4IO
import BSP.Tests.UART.TestApp (echoPrompt)
import BSP.Tests.UART.Buffer

import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw as CL
import qualified SMACCMPilot.Comm.Ivory.Types.ArmingMode as A

app :: (e -> PX4Platform)
    -> Tower e ()
app topx4 = do

  control_law <- channel
  motor_output <- channel
  px4io_state <- channel

  env <- getEnv
  case px4platform_px4io (topx4 env) of
    PX4IO_Serial dmauart pins conf -> px4ioTower tocc dmauart pins conf (snd control_law) (snd motor_output) (fst px4io_state)
    PX4IO_None -> error "Cannot build PX4IO Test: not supported on this platform"

  monitor "stub" $ do
    handler (snd px4io_state) "new_px4io_state" $
      callback $ const $ return () -- I need a place to put the breakpoint...


  (buffered_uarto, uarti, mon) <- px4ConsoleTower topx4
  monitor "console_uart" mon

  unbuffered_uarto <- uartUnbuffer buffered_uarto

  arming_control <- channel
  echoPrompt "px4io unit test" unbuffered_uarto uarti (fst arming_control)

  monitor "arming_translation" $ do
    handler (snd arming_control) "arming_control" $ do
      e <- emitter (fst control_law) 1
      callbackV $ \ a -> do
        cl <- local (istruct [CL.arming_mode .= ival (a ? (A.armed, A.safe))])
        emit e (constRef cl)
  where
  tocc = px4platform_clockconfig . topx4

