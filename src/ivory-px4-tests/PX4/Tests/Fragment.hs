{-# LANGUAGE DataKinds #-}

module PX4.Tests.Fragment where

import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.Driver.UART
import Ivory.Language
import Ivory.Serialize (serializeModule, serializeArtifacts)
import Ivory.Stdlib
import Ivory.Tower
import PX4.Tests.Platforms
import SMACCM.Fragment
import SMACCMPilot.Mavlink.CRC (mavlinkCRCModule)
import SMACCMPilot.Mavlink.Messages.SmaccmpilotNavCmd (smaccmpilotNavCmdModule)
import qualified SMACCMPilot.Mavlink.Receive as R
import SMACCMPilot.Mavlink.Send (mavlinkSendModule)
import SMACCMPilot.Mavlink.Unpack

app :: (e -> PX4Platform)
    -> Tower e ()
app topx4 = do
  mapM_ (\ m -> towerModule m >> towerDepends m)
    [ serializeModule
    , mavlinkSendModule
    , mavlinkCRCModule
    , R.mavlinkReceiveStateModule
    , smaccmpilotNavCmdModule
    ]
  mapM_ towerArtifact serializeArtifacts

  px4platform <- fmap topx4 getEnv
  let tocc = px4platform_clockconfig . topx4

  can <- maybe (fail "fragmentation test requires a CAN peripheral") return $ px4platform_can px4platform
  (_, canReq, _, _) <- canTower tocc (can_periph can) 500000 (can_RX can) (can_TX can)

  (fragReq, fragAbort, fragDone) <- fragmentSender 0x001 False canReq (Proxy :: Proxy 32)

  let uart = px4platform_console px4platform
  (istream, _) <- uartTower tocc (uart_periph uart) (uart_pins uart)
                                  115200 (Proxy :: Proxy 256)

  monitor "decode" $ do
    msg <- stateInit "msg" (izero :: Init (Struct "smaccmpilot_nav_cmd_msg"))
    in_progress <- state "in_progress"
    abort_pending <- state "abort_pending"

    coroutineHandler systemInit istream "decode_uart" $ do
      toFrag <- emitter fragReq 1
      doAbort <- emitter fragAbort 1
      return $ R.mavlinkReceiver (return ()) $ \ mavState -> do
        let (unpacker, msgid) = unpackMsg
        rxid <- deref (mavState ~> R.msgid)
        when (rxid ==? msgid) $ do
          call_ unpacker msg $ toCArray $ mavState ~> R.payload
          was_in_progress <- deref in_progress
          ifte_ was_in_progress (emitV doAbort true >> store abort_pending true) (emit toFrag (constRef msg) >> store in_progress true)

    handler fragDone "fragment_done" $ do
      toFrag <- emitter fragReq 1
      callback $ const $ do
        was_aborting <- deref abort_pending
        when was_aborting $ do
          emit toFrag $ constRef msg
          store in_progress true
          store abort_pending false
