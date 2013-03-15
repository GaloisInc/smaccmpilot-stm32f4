{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Flight.GCS.Transmit.USARTSender
  ( usartSender
  ) where

import Ivory.Language

import Ivory.BSP.HWF4.USART
import SMACCMPilot.Mavlink.Send

usartSender :: MemArea (Struct "usart")
            -> String
            -> Uint8
            -> Uint8
            -> MavlinkSender
usartSender usart_area name sysid compid = MavlinkSender $
  mavlinkSendWithWriter sysid compid sendername txseq_area (MavlinkWriteMacro write) deps
  where
  sendername = "mavlinksender_" ++ name
  txseqname = sendername ++ "_txseq"
  txseq_area :: MemArea (Stored Uint8)
  txseq_area = area txseqname (Just (ival 0))
  write :: ConstRef (Stack s) (CArray (Stored Uint8)) -- buf
        -> Uint8 -- len
        -> Ivory (Top s) () ()
  write buf len = do
    usart <- addrOf usart_area
    call_ usartWrite usart buf (safeCast len)
  deps = do
    defMemArea txseq_area
