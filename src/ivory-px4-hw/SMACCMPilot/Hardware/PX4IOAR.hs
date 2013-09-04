{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}

module SMACCMPilot.Hardware.PX4IOAR where

import Control.Monad (replicateM_)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.StateMachine

import Ivory.BSP.STM32F4.UART
import Ivory.BSP.STM32F4.GPIO

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

px4ioarTower :: (SingI n, IvoryArea a, IvoryZero a)
             => (forall s cs . ConstRef s a
                  -> Ivory (AllocEffects cs)
                       (ConstRef (Stack cs) (Array 4 (Stored IFloat))))
             -> ChannelSink n a
             -> Tower p ()
px4ioarTower decode motorChan = do
  ((_unusedRxChan :: ChannelSink 1 (Stored Uint8))
   ,(txchan :: ChannelSource 12 (Stored Uint8))) <- uartTower uart2 115200
  task "px4ioar" $ do
    ostream <- withChannelEmitter txchan "uart_ostream"
    istream <- withChannelEvent   motorChan  "motor_istream"
    motorInit <- taskLocal "motorInit"
    bootAttempts <- taskLocal "bootAttempts"
    throttle <- taskLocal "throttle"
    let put :: (Scope cs ~ GetAlloc eff) => Uint8 -> Ivory eff ()
        put = emitV_ ostream
        putbyte = liftIvory_ . put
        putPacket :: (SingI n)
                  => ConstRef s (Array n (Stored Uint8))
                  -> Ivory (AllocEffects cs) ()
        putPacket p = arrayMap $ \i ->
          noBreak $ deref (p ! i) >>= (emitV_ ostream)
        initBytes = [0xE0, 0x91, 0xA1, 0x40]
        sendMultiPacket :: (Scope cs ~ GetAlloc eff) => Ivory eff ()
        sendMultiPacket = replicateM_ 6 (put 0xA0)

    sm <- stateMachine "ioar" $ mdo
      bootBegin <- stateNamed "bootBegin" $ do
        timeout 100 $ liftIvory $ do
          select_set_all false
          store motorInit 0
          return $ goto init1
      init1 <- stateNamed "init1" $ timeout 3 $ do
        liftIvory_ $ do
          m <- deref motorInit
          when (m <? 4) $ do
            select_set m true
        putbyte (initBytes !! 0)
        putbyte (initBytes !! 1)
        goto init2
      init2 <- stateNamed "init2" $ timeout 7 $ do
        putbyte (initBytes !! 2)
        liftIvory_ $ do
          m <- deref motorInit
          emitV_ ostream (m + 1)
        putbyte (initBytes !! 3)
        goto init3
      init3 <- stateNamed "init3" $ timeout 1 $ do
        liftIvory_ $ do
          m <- deref motorInit
          select_set m false
        goto init4
      init4 <- stateNamed "init4" $ timeout 200 $ do
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
        timeout 2 $ liftIvory $ do
          t <- deref bootAttempts
          store bootAttempts (t+1)
          -- Need to try booting thrice for it to stick.  First boot tends to
          -- make motor controller leds blink, and nothing else. Second boot
          -- tends to boot motors 2,3,4. Third boot will generally boot motor 1.
          -- This comment brought to you by makers of High Assurance Software (tm)
          return $ do
            branch (t <? 2) bootBegin
            goto loop
      loop <- stateNamed "loop" $ do
        entry $ liftIvory_ $ do
          arrayMap $ \i ->
            store (throttle ! i) 0.0
        period 5 $ liftIvory_ $ do
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

