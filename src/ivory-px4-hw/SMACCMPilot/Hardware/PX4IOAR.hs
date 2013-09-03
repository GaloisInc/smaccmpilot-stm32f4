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
  where
  sel acc (ind,pin) = ifte_ ((fromIntegral ind) ==? pnum) (act pin) acc
  -- Active Low:
  act pin = ifte_ v (pinClear pin) (pinSet pin)

select_set_all :: IBool -> Ivory eff ()
select_set_all v = mapM_ act select_pins
  where -- Active Low:
  act pin = ifte_ v (pinClear pin) (pinSet pin)

px4ioarTower :: (SingI n)
             => ChannelSink n (Array 4 (Stored IFloat))
             -> Tower p ()
px4ioarTower motorChan = do
  ((_unusedRxChan :: ChannelSink 1 (Stored Uint8))
   ,(txchan :: ChannelSource 12 (Stored Uint8))) <- uartTower uart2 115200
  task "px4ioar" $ do
    ostream <- withChannelEmitter txchan "uart_ostream"
    istream <- withChannelEvent   motorChan  "motor_istream"
    motorInit <- taskLocal "motorInit"
    bootAttempts <- taskLocal "bootAttempts"
    let put :: (Scope cs ~ GetAlloc eff) => Uint8 -> Ivory eff ()
        put = emitV_ ostream
        putbyte = liftIvory_ . put
        initBytes = [0xE0, 0x91, 0xA1, 0x40]
        sendMultiPacket :: (Scope cs ~ GetAlloc eff) => Ivory eff ()
        sendMultiPacket = replicateM_ 6 (put 0xA0)

        turnM1OnLow :: (Scope cs ~ GetAlloc eff) => Ivory eff ()
        turnM1OnLow = do
          put 0x20
          put 0xA0
          put 0x00
          put 0x00
          put 0x00
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
          sendMultiPacket
          sendMultiPacket
        goto bootCheckComplete
      bootCheckComplete <- stateNamed "bootCheckComplete" $
        timeout 2 $ liftIvory $ do
          t <- deref bootAttempts
          store bootAttempts (t+1)
          return $ do
            branch (t <? 2) bootBegin
            goto loop
      loop <- stateNamed "loop" $ timeout 5 $ do
        liftIvory_ turnM1OnLow
        goto loop
      return bootBegin

    taskInit $ do
      pins_init
      pinSet muxdir_pin
      store bootAttempts (0::Uint32)
      begin sm

 -- change motor mixer [0.0f..1.0f] range to to [0..500]
scale_period :: IFloat -> Uint16
scale_period f = castWith 0 (500*f)

scale_motors :: ConstRef s (Array 4 (Stored IFloat)) -> Ivory eff (ConstRef s' (Array 4 (Stored Uint16)))
scale_motors ms = undefined

ardrone_motor_set :: ConstRef s (Array 4 (Stored Uint16)) -> Ivory eff ()
ardrone_motor_set periods = return () -- XXX

ardrone_pins_init :: Ivory eff ()
ardrone_pins_init = return () -- XXX

-- motor_init: state machine that requires timed waiting
