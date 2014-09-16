{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module BSP.Tests.CAN.TestApp where

import Ivory.Language
import Ivory.Tower

import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.Peripheral.CAN.Filter
import Ivory.BSP.STM32.PlatformClock

import BSP.Tests.LED
import BSP.Tests.Platforms

app :: forall p
     . (ColoredLEDs p, PlatformClock p, TestCAN p, BoardInitializer p)
    => Tower p ()
app = do
  boardInitializer

  (req, res) <- canTower (testCAN platform) 500000 (testCANRX platform) (testCANTX platform)

  task "simplecontroller" $ do
    taskInit $ do
      let emptyID = CANFilterID32 (fromRep 0) (fromRep 0) False False
      canFilterInit (testCANFilters platform) [CANFilterBank CANFIFO0 CANFilterMask $ CANFilter32 emptyID emptyID] []
      ledSetup $ redLED platform
      ledOn $ redLED platform

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

    received <- taskLocalInit "can_received_count" (ival (0 :: Uint32))
    handle res_event "result" $ \_ -> do
      count <- deref received
      store received (count + 1)
      ifte_ (count .& 1 ==? 0)
        (ledOff $ redLED platform)
        (ledOn $ redLED platform)
  where
  platform = Proxy :: Proxy p
