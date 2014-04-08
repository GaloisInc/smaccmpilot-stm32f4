{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module UARTStressTest where

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import Platforms
import LEDTower

import Ivory.BSP.STM32F4.GPIO
import Ivory.BSP.STM32F4.UART
import Ivory.BSP.STM32F4.UART.Tower
import Ivory.BSP.STM32F4.RCC
import Ivory.BSP.STM32F4.Signalable

ready :: Uint8
ready = 0

sent :: Uint8
sent = 1

got :: Uint8
got = 2

failed_timeout :: Uint8
failed_timeout = 3

failed_badval :: Uint8
failed_badval = 3

garbage_vals :: Uint8
garbage_vals = 4


loopbackMonitor :: ChannelSource (Stored Uint8)
                -> ChannelSink (Stored Uint8)
                -> GPIOPin
                -> String
                -> Uint8
                -> Tower p ()
loopbackMonitor src snk p n correctval = task (named "loopbackMonitor") $ do
  timer <- timerEvent (Milliseconds 2)
  out <- withChannelEmitter  src "out"
  inp <- withChannelReceiver snk "in"

  success <- taskLocalInit (named "success") (ival (0 :: Uint32))
  state   <- taskLocalInit (named "state")   (ival ready)
  time    <- taskLocal     (named "time")

  taskInit $ do
    ledSetup
    ledOff

  handleV timer "periodic" $ \t -> do
    s <- deref state
    cond_
      [ s ==? ready ==> do
          -- Flush:
          arrayMap $ \(_v :: Ix 99) -> do
            (_, _) <- receiveV inp
            return ()
          -- Send one byte:
          emitV_ out correctval
          store time t
          store state sent

      , s ==? sent ==> do
          sendtime <- deref time
          when (t >? (sendtime + (fromIMilliseconds (2 :: Sint64)))) $ do
            (rxd, val) <- receiveV inp
            when rxd $ do
              store time t
              ifte_ (val ==? correctval)
                (store state got)
                (store state failed_badval)
          when (t >? (sendtime + (fromIMilliseconds (4 :: Sint64)))) $ do
            store time t
            (store state failed_timeout)

      , s ==? got  ==> do
          gottime <- deref time
          when (t >? (gottime + (fromIMilliseconds (4 :: Sint64)))) $ do
            a <- local (ival false)
            arrayMap $ \(_v :: Ix 99) -> do
              (rxd, _) <- receiveV inp
              when rxd $ store a true
            anythingelse <- deref a
            ifte_ anythingelse
              (store state garbage_vals)
              (ledOn >> success %= (+1) >> store state ready)
      , s ==? failed_badval ==> do
          ledOff
      , s ==? failed_timeout ==> do
          ledOff
      , s ==? garbage_vals ==> do
          ledOff
      ]
  where
  named s = n ++ "_" ++ s
  ledSetup :: Ivory eff ()
  ledSetup = do
    pinEnable        p
    pinSetOutputType p gpio_outputtype_pushpull
    pinSetSpeed      p gpio_speed_50mhz
    pinSetPUPD       p gpio_pupd_none
  ledOff :: Ivory eff ()
  ledOff = do
    -- pinSetMode       p gpio_mode_analog
    pinClear         p
    pinSetMode       p gpio_mode_output
  ledOn :: Ivory eff ()
  ledOn = do
    pinSet           p
    pinSetMode       p gpio_mode_output

app :: forall p . (ColoredLEDs p, BoardHSE p, STM32F4Signal p) => Tower p ()
app = do
  -- XXX doesn't uartTower return source, sink in the wrong pairing?

  uart3streams <- uartTower uart3 57600 (Proxy :: Proxy 256)
  uart4streams <- uartTower uart4 57600 (Proxy :: Proxy 256)

  (u1snk, u1src) <- uartTower uart1 57600 (Proxy :: Proxy 256)
  loopbackMonitor u1src u1snk pinD12 "uart1" 0x66

  (u2snk, u2src) <- uartTower uart2 57600 (Proxy :: Proxy 256)
  loopbackMonitor u2src u2snk pinD13 "uart2" 0x77



