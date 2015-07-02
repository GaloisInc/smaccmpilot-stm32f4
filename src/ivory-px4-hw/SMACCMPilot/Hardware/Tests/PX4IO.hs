{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Hardware.Tests.PX4IO (app) where

import Ivory.Language
import Ivory.Tower

import Ivory.Tower.HAL.Bus.Interface
import Ivory.BSP.STM32.Driver.UART.DMA

import SMACCMPilot.Hardware.Tests.Platforms
import SMACCMPilot.Hardware.PX4IO.CRC
import SMACCMPilot.Hardware.PX4IO.Types
import SMACCMPilot.Hardware.PX4IO.Pack

app :: (e -> PX4Platform)
    -> Tower e ()
app topx4 = do
  e <- getEnv
  case px4platform_px4io (topx4 e) of
    PX4IO_None -> fail "this platform does not support PX4IO"
    PX4IO_Serial dmauart pins -> do
      (BackpressureTransmit ser_tx_req ser_tx_res, (ser_rx :: ChanOutput PX4IOBuffer))
        <- dmaUARTTower' tocc dmauart pins 1500000

      p <- period (Milliseconds 1000)
      monitor "tx_control" $ do
        monitorModuleDef $ depend px4ioPackModule
        handler p "tx_per" $ do
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

        handler ser_tx_res "tx_result" $ do
          callback $ \_r -> do
            comment "placeholder"

      monitor "rx_stub" $ do
        handler ser_rx "ser_rx" $ do
          callback $ \r -> do
            unpacked <- local izero
            v <- call px4io_unpack r unpacked
            assert (v ==? 0)

  mapM_ towerModule mods
  mapM_ towerDepends mods
  where
  tocc = px4platform_clockconfig . topx4

  mods =
    [ px4ioPackModule
    , px4ioCRCModule
    , px4ioBufferTypesModule
    , px4ioRequestTypesModule
    ]

{-
  ppmOut <- ppmTower (px4platform_ppm . topx4)
                     (px4platform_clockconfig . topx4)

  monitor "ppmsender" $ do
    ppmSender ppmOut uarto

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts

ppmSender :: Sender e (Array 8 (Stored Uint16))
ppmSender = sampleSender 'P' (Proxy :: Proxy 16)
-}
