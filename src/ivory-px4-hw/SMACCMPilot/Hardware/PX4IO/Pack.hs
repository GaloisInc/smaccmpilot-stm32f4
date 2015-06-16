{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables#-}

module SMACCMPilot.Hardware.PX4IO.Pack where

import Ivory.Language
import Ivory.Stdlib

import SMACCMPilot.Hardware.PX4IO.CRC
import SMACCMPilot.Hardware.PX4IO.Types.Request
import SMACCMPilot.Hardware.PX4IO.Types.RequestCode
import SMACCMPilot.Hardware.PX4IO.Types.Buffer

px4ioPackModule :: Module
px4ioPackModule = package "px4io_pack" $ do
  depend px4ioCRCModule
  incl px4io_pack

px4io_pack :: Def('[ Ref      s1 PX4IOBuffer
                   , ConstRef s2 (Struct "px4io_request")
                   ] :-> IBool)
px4io_pack = proc "px4io_pack" $ \buf req -> body $ do
  crc <- local izero
  ixref <- local izero
  let push val = do
        ix <- deref ixref
        store ((buf ~> stringDataL) ! ix) val
        store ixref (ix + 1)
        call_ crc8_update crc val

  rc <- deref (req ~> req_code)
  c  <- deref (req ~> count)
  when (c >? 32) (ret false)

  push (unRequestCode rc .| c)
  push 0 -- Will replace with CRC.
  push =<< deref (req ~> page)
  push =<< deref (req ~> offs)

  arrayMap $ \(reg_ix :: Ix 32) -> when (fromIx reg_ix <? safeCast c) $ do
    r <- deref ((req ~> regs) ! reg_ix)
    let (lsb, rest) = extractByte r
        (msb, _)    = extractByte rest
    push msb
    push lsb

  deref crc >>= store ((buf ~> stringDataL) ! 1)
  final_ix <- deref ixref
  store (buf ~> stringLengthL) (fromIx final_ix)

  ret true

