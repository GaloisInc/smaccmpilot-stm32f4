{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Hardware.Tests.PX4IO (app) where

import Ivory.Language
import Ivory.Tower
import Ivory.Serialize

import Ivory.Tower.HAL.Bus.Interface
import Ivory.BSP.STM32.Driver.UART.DMA.Synchronous

import SMACCMPilot.Hardware.Tests.Platforms
import SMACCMPilot.Hardware.PX4IO.CRC
import SMACCMPilot.Hardware.PX4IO.Types
import SMACCMPilot.Hardware.PX4IO.Types.Regs
import SMACCMPilot.Hardware.PX4IO.Pack
import SMACCMPilot.Comm.Ivory.Types.Px4ioState
import SMACCMPilot.Comm.Ivory.Types

app :: (e -> PX4Platform)
    -> Tower e ()
app topx4 = do
  env <- getEnv

  state_chan <- channel

  case px4platform_px4io (topx4 env) of
    PX4IO_None -> fail "this platform does not support PX4IO"
    PX4IO_Serial dmauart pins -> do
      (BackpressureTransmit ser_tx_req ser_rx)
        <- syncDMAUARTTower tocc dmauart pins 1500000

      p <- period (Milliseconds 1000)
      monitor "px4io_driver" $ do
        monitorModuleDef $ depend px4ioPackModule
        handler p "px4io_request" $ do
          (tx_e :: Emitter PX4IOBuffer) <- emitter ser_tx_req 1
          callback $ const $ do
            req <- local $ istruct
              [ req_code .= ival request_read
              , count    .= ival 2
              , page     .= ival 1 -- status page
              , offs     .= ival 0
              , regs     .= iarray [ ival 0, ival 0 ]
              ]
            packed <- local izero
            v <- call px4io_pack packed (constRef req)
            assert v
            emit tx_e (constRef packed)

        px4io_success <- state "px4io_success"
        px4io_fail    <- state "px4io_fail"
        let incr r = do
              v <- deref r
              store r (v + (1 :: Uint32))

        handler ser_rx "px4io_response" $ do
          e <- emitter (fst state_chan) 1
          callback $ \r -> do
            unpacked <- local izero
            v <- call px4io_unpack r unpacked

            r0 <- deref (unpacked ~> regs ! 0)
            r1 <- deref (unpacked ~> regs ! 1)
            s <- local $ istruct [ status .= px4ioStatusIval r0
                                 , alarms .= px4ioAlarmsIval r1
                                 ]

            ifte_ (v ==? 0)
              (emit e (constRef s) >> incr px4io_success)
              (incr px4io_fail)
            comment "placeholder"

  monitor "stub" $ do
    handler (snd state_chan) "new_px4io_state" $
      callback $ const $ return () -- I need a place to put the breakpoint...


  mapM_ towerModule mods
  mapM_ towerDepends mods
  mapM_ towerArtifact serializeArtifacts
  where
  tocc = px4platform_clockconfig . topx4

  mods =
    [ px4ioPackModule
    , px4ioCRCModule
    , px4ioBufferTypesModule
    , px4ioRequestTypesModule
    , serializeModule
    ] ++ typeModules

