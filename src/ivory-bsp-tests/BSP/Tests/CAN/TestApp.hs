{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module BSP.Tests.CAN.TestApp where

import Ivory.Language
import Ivory.Tower

import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.PlatformClock

import BSP.Tests.Platforms

app :: forall p
     . (ColoredLEDs p, PlatformClock p, TestCAN p, BoardInitializer p)
    => Tower p ()
app = do
  boardInitializer

  (req, res) <- canTower (testCAN platform) 500000

  task "simplecontroller" $ do
    req_emitter <- withChannelEmitter req "req"
    res_event   <- withChannelEvent   res "res"
    periodic    <- withPeriodicEvent (Milliseconds 250)
    handleV periodic "periodic" $ \p -> do
      time :: Uint64 <- assign $ signCast $ toIMicroseconds p
      r <- local $ istruct
        [ tx_id  .= ival 0x7FF
        , tx_ide .= ival false
        , tx_rtr .= ival false
        , tx_buf .= iarray [ ival $ bitCast $ time `iShiftR` fromInteger (8 * i) | i <- [7,6..0] ]
        , tx_len .= ival 8
        ]
      emit_ req_emitter $ constRef r

    handle res_event "result" $ \_ -> return () -- TODO
  where
  platform = Proxy :: Proxy p
