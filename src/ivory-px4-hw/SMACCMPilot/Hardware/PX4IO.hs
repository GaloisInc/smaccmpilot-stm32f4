{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Hardware.PX4IO where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Serialize

import Ivory.Tower.HAL.Bus.Interface
import Ivory.BSP.STM32.Driver.UART.DMA.Synchronous

import SMACCMPilot.Time
import SMACCMPilot.Hardware.PX4IO.CRC
import SMACCMPilot.Hardware.PX4IO.Types
import SMACCMPilot.Hardware.PX4IO.Types.Regs
import SMACCMPilot.Hardware.PX4IO.Pack

import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw as CL
import qualified SMACCMPilot.Comm.Ivory.Types.QuadcopterMotors as M
import qualified SMACCMPilot.Comm.Ivory.Types.ArmingMode as A
import           SMACCMPilot.Comm.Ivory.Types.Px4ioState
import           SMACCMPilot.Comm.Ivory.Types

import           Ivory.BSP.STM32.ClockConfig
import           Ivory.BSP.STM32.Peripheral.UART
import           Ivory.BSP.STM32.Peripheral.UART.DMA

data PX4IOPWMConfig =
  PX4IOPWMConfig
    { px4iopwm_min :: Int
    , px4iopwm_max :: Int
    } deriving (Eq, Show)

px4ioTower :: (e -> ClockConfig)
           -> DMAUART
           -> UARTPins
           -> PX4IOPWMConfig
           -> ChanOutput ('Struct "control_law")
           -> ChanOutput ('Struct "quadcopter_motors")
           -> ChanInput  ('Struct "px4io_state")
           -> Tower e ()
px4ioTower tocc dmauart pins pwmconfig control_law motors state_chan = do
  (BackpressureTransmit ser_tx_req ser_rx, driver_ready)
    <- syncDMAUARTTower tocc dmauart pins 1500000

  loop_p <- period (Milliseconds 20)
  init_p <- period init_wait

  init_chan <- channel

  let rpc_send
        :: (GetAlloc eff ~ 'Scope s)
        => Init ('Struct "px4io_request")
        -> Emitter ('Struct "ivory_string_PX4IOBuffer")
        -> Ivory eff IBool
      rpc_send req_ival req_e = do
        req <- local req_ival
        packed <- local izero
        packing_valid <- call px4io_pack packed (constRef req)
        emit req_e (constRef packed)
        return packing_valid

  monitor "px4io_driver" $ do
    monitorModuleDef $ depend px4ioPackModule
    px4io_success <- state "px4io_success"
    px4io_fail    <- state "px4io_fail"

    let incr r = do
          v <- deref r
          store r (v + (1 :: Uint32))

    px4io_state <- state "px4io_state_"

    arming_mode  <- stateInit "arming_mode" (ival A.safe)
    motor_output <- stateInit "motor_output" (istruct [ M.frontleft  .= ival 0
                                                      , M.frontright .= ival 0
                                                      , M.backleft   .= ival 0
                                                      , M.backright  .= ival 0
                                                      ])

    handler control_law "control_law" $ do
      callback $ \cl -> refCopy arming_mode (cl ~> CL.arming_mode)

    handler motors "motor_output" $ do
      callback $ \o -> refCopy motor_output o

    init_ready    <- state "init_deadline"
    init_deadline <- state "init_deadline"
    handler driver_ready "driver_ready" $ callbackV $ \t -> do
      store init_deadline (t + toITime init_wait)
      store init_ready    true

    handler init_p "initialize_period" $ do
      e <- emitter (fst init_chan) 1
      callbackV $ \t -> do
        ready <- deref init_ready
        when ready $ do
          deadline <- deref init_deadline
          when (t >=? deadline) $ do
            emitV e t
            store init_ready false

    loop_ready <- state "coroutine_loop_ready"

    handler loop_p "periodic_coroutine_loop_begin" $ do
      e <- emitter ser_tx_req 1
      callback $ const $ do
        rdy <- deref loop_ready
        when rdy $ do
          am <- deref arming_mode
          let req = istruct
                [ req_code .= ival request_write
                , page     .= ival 50 -- Setup page
                , count    .= ival 1 -- One reg
                , offs     .= ival 1 -- Starting at reg 1, SETUP_ARMING
                , regs     .= iarray [ ival ((am ==? A.armed) ? (3, 1)) ]
                                     -- LSBit: IO Arming permitted
                                     -- second LSBit: FMU Armed
                ]
          _ <- rpc_send req e -- Packing will always succeed.
          store loop_ready false

    coroutineHandler (snd init_chan) ser_rx "px4io" $ do
      req_e <- emitter ser_tx_req 1
      res_e <- emitter state_chan 1
      return $ CoroutineBody $ \ yield -> do
        let rpc req_ival = do
              pack_valid <- rpc_send req_ival req_e
              (response, unpack_valid) <- rpc_receive
              return (response, pack_valid .&& unpack_valid)
            rpc_receive = do
              res_packed <- yield
              res <- local izero
              unpacking_valid <- call px4io_unpack (constRef res_packed) res
              return (res, unpacking_valid ==? 0)

        -- Begin initializing:
        -- To initialize the RC outputs, IO must be disarmed.
        (_, init_setup_ok) <- rpc $ istruct
          [ req_code .= ival request_write
          , page     .= ival 50 -- Setup page
          , count    .= ival 1 -- One reg
          , offs     .= ival 1 -- Starting at reg 1, SETUP_ARMING
          , regs     .= iarray [ ival 0 ] -- IO and FMU disarmed.
          ]

        -- Initialize the RC Outputs. We are only using four channels, hence
        -- the loop bounded by Ix 4.
        init_ok_r <- local (ival init_setup_ok)
        arrayMap $ \(ix :: Ix 4) -> noBreak $ do
          chan_num <- assign (castDefault (fromIx ix))
          (_, rc_conf_ok) <- rpc $ istruct
            [ req_code .= ival request_write
            , page     .= ival 53 -- RC Config page
            , count    .= ival 1 -- One reg
            , offs     .= ival ((chan_num * 6) + 5) -- channel * stride + rc_config_options
            , regs     .= iarray [ ival 1 ] -- 1 : channel enabled
            ]
          unless rc_conf_ok (store init_ok_r false)

        -- Done initializing. If we had any failures in here, we're screwed.
        init_ok <- deref init_ok_r

        forever $ noBreak $ do
          -- INVARIANT:
          -- forever body must terminate in a shorter duration than the period
          -- used to trigger the first transaction in the body.
          -- ASSUMPTIONS:
          -- dmauart driver sends responses (ser_rx) on 1ms period thread. So,
          -- worst case execution time is 2ms * number of rpcs (10ms)

          -- NOTE: this rpc_recieve is for a transaction kicked off by the
          -- periodic handler above. It must be surrounded by modifying the
          -- `loop_ready` state in order to ensure the backpressure transmit
          -- protocol is always followed.
          store loop_ready true
          -- Periodic handler sets loop_ready back to false.
          (_, setup_ok) <- rpc_receive

          output_regs <- outRegIval pwmconfig (constRef motor_output)
          (_, output_ok) <- rpc $ istruct
            [ req_code .= ival request_write
            , page     .= ival 54 -- Direct PWM Page
            , count    .= ival 4 -- First four registers
            , offs     .= ival 0 -- Starting at reg 0
            , regs     .= output_regs
            ]

          (status_regs, status_ok) <- rpc $ istruct
            [ req_code .= ival request_read
            , page     .= ival 1 -- Status page
            , count    .= ival 2 -- two registers
            , offs     .= ival 2 -- Starting at reg 2
            , regs     .= iarray [ ival 0, ival 0 ]
            ]
          status_flags <- deref (status_regs ~> regs ! 0)
          status_alarms <- deref (status_regs ~> regs ! 1)

          px4ioStatusFromReg status_flags (px4io_state ~> status)
          px4ioAlarmsFromReg status_alarms (px4io_state ~> alarms)

          -- At least in PPM Mode, input channel count is at offset 0
          -- (as expected) but then channels start at offset 6
          -- in the RAW_RC_INPUT page. We need the first 6 channels.
          (rc_input_reg, rc_input_ok) <- rpc $ istruct
            [ req_code .= ival request_read
            , page     .= ival 4 -- RAW_RC_INPUT page
            , count    .= ival 12 -- Need register 0 and from 6..11
            , offs     .= ival 0 -- Starting at reg 0
            , regs     .= iarray [ ival 0 ]
            ]
          t <- fmap timeMicrosFromITime getTime

          rcInputFromRegs t rc_input_reg (px4io_state ~> rc_in)

          store (px4io_state ~> time) t

          ifte_ (init_ok .&& setup_ok .&& output_ok .&& status_ok
                .&& rc_input_ok)
            (store (px4io_state ~> comm_ok) true  >> incr px4io_success)
            (store (px4io_state ~> comm_ok) false >> incr px4io_fail)

          emit res_e (constRef px4io_state)

  mapM_ towerModule mods
  mapM_ towerDepends mods
  mapM_ towerArtifact serializeArtifacts

  where

  mods =
    [ px4ioPackModule
    , px4ioCRCModule
    , px4ioBufferTypesModule
    , px4ioRequestTypesModule
    , serializeModule
    ] ++ typeModules

  init_wait = Milliseconds 1000


outRegIval :: PX4IOPWMConfig
           -> ConstRef s ('Struct "quadcopter_motors")
           -> Ivory eff (Init ('Array 32 ('Stored Uint16)))
outRegIval pwmconfig motors = do
  fl <- deref (motors ~> M.frontleft)
  fr <- deref (motors ~> M.frontright)
  bl <- deref (motors ~> M.backleft)
  br <- deref (motors ~> M.backright)
  return (iarray [ ival (scale fr)
                 , ival (scale bl)
                 , ival (scale fl)
                 , ival (scale br)
                 ])
  where
  scale :: IFloat -> Uint16
  scale t = (t <? idle) ? (minPWM, castWith 0 ((t * range) + (safeCast minPWM)))

  minPWM = fromIntegral (px4iopwm_min pwmconfig)
  maxPWM = fromIntegral (px4iopwm_max pwmconfig)
  range :: IFloat
  range = safeCast (maxPWM - minPWM)
  idle = 0.07


