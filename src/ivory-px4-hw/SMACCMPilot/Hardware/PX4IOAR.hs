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
pins_init = do mapM_ pin_init (muxdir_pin:select_pins)
  where
  pin_init p = do
    pinEnable        p
    pinSetOutputType p gpio_outputtype_pushpull
    pinSetSpeed      p gpio_speed_2mhz
    pinSetPUPD       p gpio_pupd_none

select_set :: Uint8 -> IBool -> Ivory eff ()
select_set pnum v = foldr sel (return ()) (zip [(0::Int)..] select_pins)
  where
  sel (ind,pin) acc = ifte_ ((fromIntegral ind) ==? pnum) (act pin) acc
  act pin = ifte_ v (pinSet pin) (pinClear pin)

select_set_all :: IBool -> Ivory eff ()
select_set_all v = mapM_ act select_pins
  where
  act pin = ifte_ v (pinSet pin) (pinClear pin)

px4ioarTower :: (SingI n)
             => ChannelSink n (Array 4 (Stored IFloat))
             -> Tower p ()
px4ioarTower motorChan = do
  ((_unusedRxChan :: ChannelSink 1 (Stored Uint8))
   ,(txchan :: ChannelSource 12 (Stored Uint8))) <- uartTower uart2 115200
  task "px4ioar" $ do
    ostream <- withChannelEmitter txchan "uart_ostream"
    istream <- withChannelEvent   motorChan  "motor_istream"
    motorInit <- taskLocalInit "motorInit" (ival (0 :: Uint8))
    let put :: (Scope cs ~ GetAlloc eff) => Uint8 -> Ivory eff ()
        put = emitV_ ostream
        putbyte = liftIvory_ . put
        initBytes = [0xE0, 0x91, 0xA1, 0x40]
        sendMultiPacket :: (Scope cs ~ GetAlloc eff) => Ivory eff ()
        sendMultiPacket = replicateM_ 5 (put 0xA0)
    sm <- stateMachine "ioar" $ mdo
      init0 <- stateNamed "init0" $ entry $ do
        liftIvory $ do
          m <- deref motorInit
          when (m <? 4) $ do
            select_set m true
          return $ goto init1
      init1 <- stateNamed "init1" $ timeout 1 $ do
        putbyte (initBytes !! 0)
        goto init2
      init2 <- stateNamed "init2" $ timeout 1 $ do
        putbyte (initBytes !! 1)
        goto init3
      init3 <- stateNamed "init3" $ timeout 1 $ do
        putbyte (initBytes !! 2)
        goto init4
      init4 <- stateNamed "init4" $ timeout 1 $ do
        liftIvory_ $ do
          m <- deref motorInit
          emitV_ ostream (m + 1)
        goto init5
      init5 <- stateNamed "init5" $ timeout 1 $ do
        putbyte (initBytes !! 3)
        goto init6
      init6 <- stateNamed "init6" $ timeout 1 $ do
        liftIvory_ $ do
          m <- deref motorInit
          select_set m false
        goto init7
      init7 <- stateNamed "init7" $ timeout 200 $ do
        liftIvory $ do
          m <- deref motorInit
          store motorInit (m+1)
          return $ do
            branch (m >=? 3) initMulti1
            goto init0
      initMulti1 <- stateNamed "initMulti1" $ entry $ do
        liftIvory_ $ select_set_all true
        goto initMulti2
      initMulti2 <- stateNamed "initMulti2" $ timeout 1 $ do
        liftIvory_ $ do
          sendMultiPacket
        goto initMulti3
      initMulti3 <- stateNamed "initMulti3" $ timeout 2 $ do
        liftIvory_ $ do
          sendMultiPacket
        goto initMulti4
      initMulti4 <- stateNamed "initMulti4" $ timeout 2 $ do
        goto loop
      loop <- stateNamed "loop" $ entry $ do -- XXX implement
        goto loop
      return init0

    taskInit $ do
      pins_init
      pinSet muxdir_pin
      select_set_all false
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
