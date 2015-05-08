{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Datalink.CAN
  ( canDatalink
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN.Fragment
import Ivory.Tower.HAL.Bus.Interface
import SMACCMPilot.Commsec.Sizes

gidlType :: MessageType PlaintextArray
gidlType = messageType 0x100 False (Proxy :: Proxy 80)

canDatalink :: AbortableTransmit (Struct "can_message") (Stored IBool)
            -> ChanOutput (Struct "can_message")
            -> (ChanOutput PlaintextArray -> Tower e (a, ChanOutput PlaintextArray))
            -> Tower e a
canDatalink tx rx k = do
  (assembled, fromFrag) <- channel
  fragmentReceiver rx [fragmentReceiveHandler assembled gidlType]
  (a, toFrag) <- k fromFrag
  fragmentSenderBlind toFrag gidlType tx
  return a
