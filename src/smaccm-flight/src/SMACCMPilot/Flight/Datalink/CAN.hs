{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Datalink.CAN
  ( canDatalink
  , s2cType
  , c2sType
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN.Fragment
import Ivory.Tower.HAL.Bus.Interface
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Flight.Datalink.UART (frameBuffer)

s2cType :: MessageType PlaintextArray
s2cType = messageType 0x100 False (Proxy :: Proxy 80)

c2sType :: MessageType PlaintextArray
c2sType = messageType 0x200 False (Proxy :: Proxy 80)

canDatalink :: AbortableTransmit (Struct "can_message") (Stored IBool)
            -> ChanOutput (Struct "can_message")
            -> (ChanOutput PlaintextArray -> Tower e (a, ChanOutput PlaintextArray))
            -> Tower e a
canDatalink tx rx k = do
  buffered_rx <- frameBuffer rx (Milliseconds 1)
                                (Proxy :: Proxy 12)
  (assembled, fromFrag) <- channel
  fragmentReceiver buffered_rx [fragmentReceiveHandler assembled c2sType]

  (a, toFrag) <- k fromFrag
  fragmentSenderBlind toFrag s2cType tx
  return a
