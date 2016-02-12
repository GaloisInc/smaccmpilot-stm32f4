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

canDatalink :: AbortableTransmit ('Struct "can_message") ('Stored IBool)
            -> ChanOutput ('Struct "can_message")
            -> ChanInput PlaintextArray
            -> ChanOutput PlaintextArray
            -> Tower e ()
canDatalink can_tx can_rx msg_rx msg_tx = do
  buffered_rx <- frameBuffer can_rx (Milliseconds 1)
                                    (Proxy :: Proxy 12)
  fragmentReceiver buffered_rx [fragmentReceiveHandler msg_rx c2sType]

  fragmentSenderBlind msg_tx s2cType can_tx
