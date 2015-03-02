{-# LANGUAGE DataKinds #-}

module PX4.Tests.Fragment where

import BSP.Tests.Platforms
import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.Driver.UART
import Ivory.Language
import Ivory.Serialize (serializeModule, serializeArtifacts)
import Ivory.Stdlib
import Ivory.Tower
import SMACCM.Fragment
import SMACCMPilot.Mavlink.CRC (mavlinkCRCModule)
import SMACCMPilot.Mavlink.Messages.SmaccmpilotNavCmd (smaccmpilotNavCmdModule)
import qualified SMACCMPilot.Mavlink.Receive as R
import SMACCMPilot.Mavlink.Send (mavlinkSendModule)
import SMACCMPilot.Mavlink.Unpack

app :: (e -> ClockConfig)
    -> (e -> TestCAN)
    -> (e -> TestUART)
    -> Tower e ()
app tocc tocan touart = do
  mapM_ (\ m -> towerModule m >> towerDepends m)
    [ serializeModule
    , mavlinkSendModule
    , mavlinkCRCModule
    , R.mavlinkReceiveStateModule
    , smaccmpilotNavCmdModule
    ]
  mapM_ towerArtifact serializeArtifacts

  env <- getEnv

  let can = tocan env
  (_, canReq, _, _) <- canTower tocc (testCAN can) 500000 (testCANRX can) (testCANTX can)

  (fragReq, fragAbort, fragDone) <- fragmentSender 0x001 False canReq (Proxy :: Proxy 32)

  let uart = touart env
  (istream, _) <- uartTower tocc (testUARTPeriph uart) (testUARTPins uart)
                                  115200 (Proxy :: Proxy 256)

  monitor "decode" $ do
    mavState <- stateInit "state" (istruct [ R.status .= ival R.status_IDLE ])
    msg <- stateInit "msg" (izero :: Init (Struct "smaccmpilot_nav_cmd_msg"))
    in_progress <- state "in_progress"
    abort_pending <- state "abort_pending"

    handler istream "decode_uart" $ do
      toFrag <- emitter fragReq 1
      doAbort <- emitter fragAbort 1
      callbackV $ \ b -> do
        R.mavlinkReceiveByte mavState b
        s <- deref (mavState ~> R.status)
        cond_
          [ (s ==? R.status_GOTMSG) ==> do
            let (unpacker, msgid) = unpackMsg
            rxid <- deref (mavState ~> R.msgid)
            when (rxid ==? msgid) $ do
              call_ unpacker msg $ toCArray $ constRef $ mavState ~> R.payload
              was_in_progress <- deref in_progress
              ifte_ was_in_progress (emitV doAbort true >> store abort_pending true) (emit toFrag (constRef msg) >> store in_progress true)
            R.mavlinkReceiveReset mavState
          , (s ==? R.status_FAIL)   ==>
            store (mavState ~> R.status) R.status_IDLE
          ]

    handler fragDone "fragment_done" $ do
      toFrag <- emitter fragReq 1
      callback $ const $ do
        was_aborting <- deref abort_pending
        when was_aborting $ do
          emit toFrag $ constRef msg
          store in_progress true
          store abort_pending false
