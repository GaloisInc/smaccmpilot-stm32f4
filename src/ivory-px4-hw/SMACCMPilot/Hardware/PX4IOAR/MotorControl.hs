{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module SMACCMPilot.Hardware.PX4IOAR.MotorControl
  ( motorControlTower
  ) where

import Control.Monad (replicateM_)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.StateMachine

import Ivory.BSP.STM32F405.UART
import Ivory.BSP.STM32F405.UART.Tower
import Ivory.BSP.STM32F405.GPIO
import qualified Ivory.BSP.STM32F405.Interrupt as F405

import Ivory.BSP.STM32.Signalable
import Ivory.BSP.STM32.BoardHSE

select_pins :: [GPIOPin]
select_pins = [ pinC4, pinC5, pinA0, pinA1 ]

muxdir_pin :: GPIOPin
muxdir_pin = pinC13

pins_init :: Ivory eff ()
pins_init = do
  pin_init muxdir_pin
  mapM_ pin_init select_pins
  where
  pin_init p = do
    pinEnable        p
    pinSet           p -- Pulled high by resistors, ensure stays high
    pinSetMode       p gpio_mode_output
    pinSetOutputType p gpio_outputtype_pushpull
    pinSetSpeed      p gpio_speed_2mhz
    pinSetPUPD       p gpio_pupd_none

select_set :: Uint8 -> IBool -> Ivory eff ()
select_set pnum v = foldl sel (return ()) (zip [(0::Int)..] select_pins)
  where -- Active Low:
  act pin = ifte_ v (pinClear pin) (pinSet pin)
  sel acc (ind,pin) = ifte_ ((fromIntegral ind) ==? pnum) (act pin) acc

select_set_all :: IBool -> Ivory eff ()
select_set_all v = mapM_ act select_pins
  where -- Active Low:
  act pin = ifte_ v (pinClear pin) (pinSet pin)

motorControlTower :: ( IvoryArea a, IvoryZero a, BoardHSE p
                     , STM32Signal F405.Interrupt p)
             => (forall s cs . ConstRef s a
                  -> Ivory (AllocEffects cs)
                       (ConstRef (Stack cs) (Array 4 (Stored IFloat))))
             -> ChannelSink a
             -> Tower p ()
motorControlTower decode motorChan = do
  (_unusedRxChan, txchan, flushchan) <- uartTowerFlushable uart2 115200
                                          (Proxy :: Proxy 12)
  task "px4ioar" $ do
    ostream <- withChannelEmitter txchan "uart_ostream"
    istream <- withChannelEvent   motorChan  "motor_istream"
    flushemitter <- withChannelEmitter flushchan "uart_flush"
    motorInit    <- taskLocal "motorInit"
    bootAttempts <- taskLocal "bootAttempts"
    throttle     <- taskLocal "throttle"
    let flush :: (Scope cs ~ GetAlloc eff) => Ivory eff ()
        flush = emitV_ flushemitter 0
        put :: (Scope cs ~ GetAlloc eff) => Uint8 -> Ivory eff ()
        put b = emitV_ ostream b
        putbyte b = liftIvory_ (put b >> flush)
        putPacket :: (SingI n)
                  => ConstRef s (Array n (Stored Uint8))
                  -> Ivory (AllocEffects cs) ()
        putPacket p = do
          arrayMap $ \i ->
            noBreak $ deref (p ! i) >>= (emitV_ ostream)
          flush
        initBytes = [0xE0, 0x91, 0xA1, 0x40]
        sendMultiPacket :: (Scope cs ~ GetAlloc eff) => Ivory eff ()
        sendMultiPacket = do
          replicateM_ 6 (put 0xA0)
          flush

    sm <- stateMachine "ioar" $ mdo
      bootBegin <- stateNamed "bootBegin" $ do
        timeout (Milliseconds 100) $ liftIvory $ do
          select_set_all false
          store motorInit 0
          return $ goto init1
      init1 <- stateNamed "init1" $ timeout (Milliseconds 10) $ do
        liftIvory_ $ do
          m <- deref motorInit
          when (m <? 4) $ do
            select_set m true
        putbyte (initBytes !! 0)
        putbyte (initBytes !! 1)
        goto init2
      init2 <- stateNamed "init2" $ timeout (Milliseconds 7) $ do
        putbyte (initBytes !! 2)
        liftIvory_ $ do
          m <- deref motorInit
          put (m + 1)
        putbyte (initBytes !! 3)
        goto init3
      init3 <- stateNamed "init3" $ timeout (Milliseconds 1) $ do
        liftIvory_ $ do
          m <- deref motorInit
          select_set m false
        goto init4
      init4 <- stateNamed "init4" $ timeout (Milliseconds 200) $ do
        liftIvory $ do
          m <- deref motorInit
          store motorInit (m+1)
          return $ do
            branch (m >=? 3) initMulti
            goto init1
      initMulti <- stateNamed "initMulti" $ entry $ do
        liftIvory_ $ do
          select_set_all true
          -- Send it twice for good luck.
          sendMultiPacket
          sendMultiPacket
        goto bootCheckComplete
      bootCheckComplete <- stateNamed "bootCheckComplete" $
        timeout (Milliseconds 2) $ liftIvory $ do
          t <- deref bootAttempts
          store bootAttempts (t+1)
          -- Need to try booting a bunch of times for it to work reliably.
          -- Honestly, I'm not sure why, with the old tower/stm32f4 uart driver,
          -- which had deterministic first-byte-delivery latency, it took three
          -- retries to work. Now it takes five retries to work.
          return $ do
            branch (t <? 8) bootBegin
            goto loop
      loop <- stateNamed "loop" $ do
        entry $ liftIvory_ $ do
          arrayMap $ \i ->
            store (throttle ! i) 0.0
        period (Milliseconds 5) $ liftIvory_ $ do
          scaled   <- scale_motors (constRef throttle)
          packet   <- motor_packet scaled
          putPacket packet
        on istream $ \encoded -> liftIvory_ $ do
          inputs <- decode encoded
          arrayMap $ \i -> do
            input <- deref (inputs ! i)
            store (throttle ! i) input
      return bootBegin

    taskInit $ do
      pins_init
      pinSet muxdir_pin
      store bootAttempts (0::Uint32)
      begin sm

-- change motor mixer [0.0f..1.0f] range to to [0..500]
scale_period :: IFloat -> Uint16
scale_period f = castWith 0 (500*f)

scale_motors :: (GetAlloc eff ~ Scope cs)
             => ConstRef s (Array 4 (Stored IFloat))
             -> Ivory eff (ConstRef (Stack cs) (Array 4 (Stored Uint16)))
scale_motors ms = do
  out <- local (iarray [ival 0, ival 0, ival 0, ival 0])
  arrayMap $ \i -> do
    m <- deref (ms ! i)
    store (out ! i) (scale_period m)
  return (constRef out)

motor_packet :: (GetAlloc eff ~ Scope cs)
             => ConstRef s (Array 4 (Stored Uint16))
             -> Ivory eff (ConstRef (Stack cs) (Array 5 (Stored Uint8)))
motor_packet ms = do
  m1 <- deref (ms ! 0)
  m2 <- deref (ms ! 1)
  m3 <- deref (ms ! 2)
  m4 <- deref (ms ! 3)
  let (m1h, m1l) = hiLo $ shiftUp m1 4
      (m2h, m2l) = hiLo $ shiftUp m2 3
      (m3h, m3l) = hiLo $ shiftUp m3 2
      (m4h, m4l) = hiLo $ shiftUp m4 1
      b0 = 0x20 .| m1h
      b1 = m1l  .| m2h
      b2 = m2l  .| m3h
      b3 = m3l  .| m4h
      b4 = m4l
  out <- local (iarray [ival b0, ival b1, ival b2, ival b3, ival b4])
  return (constRef out)
  where
  hiLo :: Uint16 -> (Uint8, Uint8)
  hiLo a = (ubits a, lbits a)
  shiftUp :: Uint16 -> Uint16 -> Uint16
  shiftUp m s = (m .& 0x01FF) `iShiftL` s

