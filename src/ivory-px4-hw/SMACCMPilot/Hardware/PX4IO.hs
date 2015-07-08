{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Hardware.PX4IO where

import Ivory.Language
import Ivory.Tower
import Ivory.Serialize

import Ivory.Tower.HAL.Bus.Interface
import Ivory.BSP.STM32.Driver.UART.DMA.Synchronous

import SMACCMPilot.Hardware.PX4IO.CRC
import SMACCMPilot.Hardware.PX4IO.Types
import SMACCMPilot.Hardware.PX4IO.Types.Regs
import SMACCMPilot.Hardware.PX4IO.Pack
import SMACCMPilot.Comm.Ivory.Types.Px4ioState
import SMACCMPilot.Comm.Ivory.Types

import           Ivory.BSP.STM32.ClockConfig
import           Ivory.BSP.STM32.Peripheral.UART
import           Ivory.BSP.STM32.Peripheral.UART.DMA

px4ioTower :: (e -> ClockConfig)
           -> DMAUART
           -> UARTPins
           -> Tower e (ChanOutput (Struct "px4io_state"))
px4ioTower tocc dmauart pins = do
  state_chan <- channel
  (BackpressureTransmit ser_tx_req ser_rx) <- syncDMAUARTTower tocc dmauart pins 1500000

  p <- period (Milliseconds 10)

  monitor "px4io_driver" $ do
    monitorModuleDef $ depend px4ioPackModule
    px4io_success <- state "px4io_success"
    px4io_fail    <- state "px4io_fail"
    let incr r = do
          v <- deref r
          store r (v + (1 :: Uint32))

    coroutineHandler p ser_rx "px4io" $ do
      req_e <- emitter ser_tx_req 1
      res_e <- emitter (fst state_chan) 1
      return $ CoroutineBody $ \ yield -> do
        let rpc req = emit req_e req >> yield
        comment ""

        status_req <- local $ istruct
          [ req_code .= ival request_read
          , count    .= ival 2
          , page     .= ival 1 -- status page
          , offs     .= ival 0
          , regs     .= iarray [ ival 0, ival 0 ]
          ]

        status_packed <- local izero
        v <- call px4io_pack status_packed (constRef status_req)
        assert v

        status_result <- rpc (constRef status_packed)

        status_unpacked <- local izero
        v2 <- call px4io_unpack (constRef status_result) status_unpacked

        r0 <- deref (status_unpacked ~> regs ! 0)
        r1 <- deref (status_unpacked ~> regs ! 1)
        s <- local $ istruct [ status .= px4ioStatusIval r0
                             , alarms .= px4ioAlarmsIval r1
                             ]

        ifte_ (v2 ==? 0)
          (emit res_e (constRef s) >> incr px4io_success)
          (incr px4io_fail)

  mapM_ towerModule mods
  mapM_ towerDepends mods
  mapM_ towerArtifact serializeArtifacts

  return (snd state_chan)
  where

  mods =
    [ px4ioPackModule
    , px4ioCRCModule
    , px4ioBufferTypesModule
    , px4ioRequestTypesModule
    , serializeModule
    ] ++ typeModules

