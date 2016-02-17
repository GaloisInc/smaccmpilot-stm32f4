{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE TypeOperators #-}

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
  depend px4ioBufferTypesModule
  depend px4ioRequestTypesModule
  incl px4io_pack
  incl px4io_unpack

px4io_pack :: Def('[ Ref      s1 PX4IOBuffer
                   , ConstRef s2 ('Struct "px4io_request")
                   ] ':-> IBool)
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

  push (request_code_bitfield rc .| c)
  push 0
  push =<< deref (req ~> page)
  push =<< deref (req ~> offs)

  arrayMap $ \(reg_ix :: Ix 32) ->
    -- Yes, we send registers even when we do a read. Yes, I know.
    when ( fromIx reg_ix <? safeCast c) $ do
      r <- deref ((req ~> regs) ! reg_ix)
      let (lsb, rest) = extractByte r
          (msb, _)    = extractByte rest
      push lsb
      push msb

  deref crc >>= store ((buf ~> stringDataL) ! 1)
  final_ix <- deref ixref
  store (buf ~> stringLengthL) (fromIx final_ix) -- XXX not sure if this is correct or off by one

  ret true

  where
  request_code_bitfield rc = (rc ==? request_read) ? ( 0
                           , (rc ==? request_write) ? ( 0x40
                           , {- default -} 0))

px4io_unpack :: Def('[ ConstRef s1 PX4IOBuffer
                     , Ref      s2 ('Struct "px4io_request")
                     ] ':-> Uint8)
px4io_unpack = proc "px4io_unpack" $ \buf req -> body $ do
  crc <- local izero
  ixref <- local izero
  let pop_nocrc = do
        ix <- deref ixref
        v <- deref ((buf ~> stringDataL) ! ix)
        store ixref (ix + 1)
        return v
      update_crc v = do
        call_ crc8_update crc v
      pop = do
        v <- pop_nocrc
        update_crc v
        return v

  count_code <- pop
  cnt <- assign (count_code .& 0x3F)
  when (cnt >? 32) (ret 1)
  store (req ~> count) cnt

  cde <- assign (count_code .& 0xC0)
  let (rc, rcvalid) = parseRequestCode cde
  unless rcvalid (ret 2)
  store (req ~> req_code) rc

  crc_pkt <- pop_nocrc
  update_crc 0

  pop >>= store (req ~> page)
  pop >>= store (req ~> offs)

  arrayMap $ \(reg_ix :: Ix 32) -> when (fromIx reg_ix <? safeCast cnt) $ do
    lsb <- pop
    msb <- pop
    v <- assign ((safeCast lsb) .| ((safeCast msb) `iShiftL` 8))
    store ((req ~> regs) ! reg_ix) v

  crc_calc <- deref crc
  when (crc_pkt /=? crc_calc) (ret 3)
  final_ix <- deref ixref

  slen <- deref (buf ~> stringLengthL)
  when (fromIx final_ix /=? slen) (ret 4) -- XXX not sure if this is correct or off by one

  ret 0
  where
  parseRequestCode c = (rc, rcvalid)
    where
    rc = (c ==? 0x00) ? (request_ok
       , (c ==? 0x40) ? (request_corrupt
       , (c ==? 0x80) ? (request_error
       , {- default -}   request_error
       )))
    rcvalid = (c ==? 0x00 .|| c ==? 0x40 .|| c ==? 0x80)


