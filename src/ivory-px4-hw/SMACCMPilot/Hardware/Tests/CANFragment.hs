{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.Tests.CANFragment
  ( app
  ) where

import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.Driver.UART
import Ivory.Language
import Ivory.Serialize (serializeModule, serializeArtifacts)
import Ivory.Stdlib
import Ivory.Tower
import SMACCMPilot.Hardware.Tests.Platforms
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

  (fragSink, fragSource) <- channel
  fragmentSenderBlind (fragSource :: ChanOutput (Struct "smaccmpilot_nav_cmd_msg")) 0x001 False canReq (Proxy :: Proxy 32)

  let uart = px4platform_console px4platform
  (istream, _) <- uartTower tocc (uart_periph uart) (uart_pins uart)
                                  115200 (Proxy :: Proxy 256)

  monitor "decode" $ do
    coroutineHandler systemInit istream "decode_uart" $ do
      toFrag <- emitter fragSink 1
      return $ R.mavlinkReceiver (return ()) $ \ mavState -> do
        let (unpacker, msgid) = unpackMsg
        rxid <- deref (mavState ~> R.msgid)
        when (rxid ==? msgid) $ do
          msg <- local izero
          call_ unpacker msg $ toCArray $ mavState ~> R.payload
          emit toFrag $ constRef msg
