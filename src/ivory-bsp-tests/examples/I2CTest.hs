{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module I2CTest where

import Ivory.Language
import Ivory.Tower

import Ivory.BSP.STM32F4.GPIO
import Ivory.BSP.STM32F4.RCC
import Ivory.BSP.STM32F4.Signalable

import Ivory.BSP.STM32F4.I2C.Peripheral
import Ivory.BSP.STM32F4.I2C.Tower

import Platforms

app ::  forall p . (ColoredLEDs p, BoardHSE p, STM32F4Signal p) => Tower p ()
app = do
  (req, res) <- i2cTower i2c1 pinB6 pinB7

  task "simplecontroller" $ do
    req_emitter <- withChannelEmitter req "req"
    res_event   <- withChannelEvent   res "res"
    periodic    <- withPeriodicEvent (Milliseconds 250)
    handleV periodic "periodic" $ \p -> do
      ifte_ ((p .% 500000) >=? 250000)
        (do r <- local $ istruct
                   [ tx_addr   .= ival 0x22
                   , tx_buf    .= iarray [ival 0xF1, ival 0xF2, ival 0xF3]
                   , tx_len    .= ival 3
                   , rx_len    .= ival 0
                   ]
            emit_ req_emitter (constRef r))
        (do r <- local $ istruct
                   [ tx_addr   .= ival 0x44
                   , tx_buf    .= iarray [ival 0xF4, ival 0xF5, ival 0xF6, ival 0xF7]
                   , tx_len    .= ival 4
                   , rx_len    .= ival 4 -- Should error, there's no device attached
                   ]
            emit_ req_emitter (constRef r))

    handle res_event "result" $ \_ -> return () -- XXX

