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

import SMACCMPilot.Time
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
  (BackpressureTransmit ser_tx_req ser_rx, driver_ready)
    <- syncDMAUARTTower tocc dmauart pins 1500000

  p <- period (Milliseconds 10)

  monitor "px4io_driver" $ do
    monitorModuleDef $ depend px4ioPackModule
    px4io_success <- state "px4io_success"
    px4io_fail    <- state "px4io_fail"
    let incr r = do
          v <- deref r
          store r (v + (1 :: Uint32))

    px4io_state <- state "px4io_state_"

    coroutineHandler driver_ready ser_rx "px4io" $ do
      req_e <- emitter ser_tx_req 1
      res_e <- emitter (fst state_chan) 1
      return $ CoroutineBody $ \ yield -> do
        let rpc req_ival = do
              req <- local req_ival
              packed <- local izero
              packing_valid <- call px4io_pack packed (constRef req)
              emit req_e (constRef packed)
              res_packed <- yield
              res <- local izero
              unpacking_valid <- call px4io_unpack (constRef res_packed) res
              return (res, packing_valid .&& (unpacking_valid ==? 0))


        (_, setup_ok) <- rpc $ istruct
          [ req_code .= ival request_write
          , page     .= ival 50 -- Setup page
          , count    .= ival 1 -- One reg
          , offs     .= ival 1 -- Starting at reg 1, SETUP_ARMING
          , regs     .= iarray [ ival 1 ] -- IO Arming OK, FMU Already Armed
          ]

        assert setup_ok

        forever $ noBreak $ do

          (status_regs, status_ok) <- rpc $ istruct
            [ req_code .= ival request_read
            , page     .= ival 1 -- Status page
            , count    .= ival 2 -- First two registers
            , offs     .= ival 0 -- Starting at reg 0
            , regs     .= iarray [ ival 0, ival 0 ]
            ]
          status_r0 <- deref (status_regs ~> regs ! 0)
          status_r1 <- deref (status_regs ~> regs ! 1)

          px4ioStatusFromReg status_r0 (px4io_state ~> status)
          px4ioAlarmsFromReg status_r1 (px4io_state ~> alarms)

          (rc_count_reg, rc_count_ok) <- rpc $ istruct
            [ req_code .= ival request_read
            , page     .= ival 4 -- RAW_RC_INPUT page
            , count    .= ival 1 -- 1 register
            , offs     .= ival 0 -- Starting at reg 0
            , regs     .= iarray [ ival 0 ]
            ]

          (rc_input_reg, rc_input_ok) <- rpc $ istruct
            [ req_code .= ival request_read
            , page     .= ival 4 -- RAW_RC_INPUT page
            , count    .= ival 6 -- 6 registers
            , offs     .= ival 6 -- Starting at RAW_RC_BASE
            , regs     .= iarray (repeat (ival 0))
            ]

          px4ioRCInputFromRegs rc_count_reg rc_input_reg (px4io_state ~> rc_in)

          t <- getTime
          store (px4io_state ~> time) (timeMicrosFromITime t)

          ifte_ (status_ok .&& rc_count_ok .&& rc_input_ok)
            (store (px4io_state ~> comm_ok) true  >> incr px4io_success)
            (store (px4io_state ~> comm_ok) false >> incr px4io_fail)

          emit res_e (constRef px4io_state)

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

