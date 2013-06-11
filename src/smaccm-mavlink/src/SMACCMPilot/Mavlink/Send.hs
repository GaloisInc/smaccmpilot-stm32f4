{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Mavlink.Send where

import Ivory.Language
import Ivory.Stdlib (unless)

import SMACCMPilot.Mavlink.CRC

type SenderMacro eff s n =  Uint8 -- id
                         -> ConstRef (Stack s) (Array n (Stored Uint8)) -- buf
                         -> Uint8 -- crcextra
                         -> Ivory eff ()

data SizedMavlinkSender n =
  SizedMavlinkSender
    { senderMacro :: forall eff s . (eff `AllocsIn` s) => SenderMacro eff s n
    , senderName  :: String
    , senderDeps  :: ModuleDef
    }

newtype MavlinkSender = MavlinkSender (forall n . (SingI n) =>  SizedMavlinkSender n)

mavlinkSenderName :: MavlinkSender -> String
mavlinkSenderName (MavlinkSender sized) = senderName (sized :: SizedMavlinkSender 1)

class MavlinkSendable t n | t -> n where
  mkSender :: SizedMavlinkSender n -> Def ('[ ConstRef s (Struct t) ] :-> ())


newtype MavlinkWriteMacro = MavlinkWriteMacro {
  unMavlinkWriteMacro :: 
    (forall eff s . (eff `AllocsIn` s)
                    => ConstRef (Stack s) (CArray (Stored Uint8)) -- buf
                    -> Uint8 -- len
                    -> Ivory eff ()) }

mavlinkChecksum :: (SingI n, eff `AllocsIn` s)
                             => ConstRef (Stack s) (Array 6 (Stored Uint8))
                             -> ConstRef (Stack s) (Array n (Stored Uint8))
                             -> Uint8
                             -> Ivory eff (Uint8, Uint8)
mavlinkChecksum header payload crcextra = do
  ck <- local (ival crc_init_v)
  arrayMap $ \i ->
    -- mavlink doesn't use the magic number
    -- in header[0] for crc calculation.
    unless (i ==? 0) $ do
      b <- deref (header ! i)
      call_ crc_accumulate b ck

  arrayMap $ \i -> do
    b <- deref (payload ! i)
    call_ crc_accumulate b ck

  call_ crc_accumulate crcextra ck
  crc_lo_hi ck

mavlinkSendWithWriter :: Uint8 -- Sysid
                      -> Uint8 -- Compid
                      -> String -- Writer name
                      -> MemArea (Stored Uint8) -- TX Sequence Number
                      -> MavlinkWriteMacro -- Ivory macro
                      -> ModuleDef -- Deps of tx seq mem area, ivory macro
                      -> MavlinkSender
mavlinkSendWithWriter sysid compid name seqnum_area cwriter writerdeps =
  MavlinkSender (SizedMavlinkSender sender name deps)
  where
  deps = do
    depend mavlinkCRCModule
    writerdeps
  write arr = (unMavlinkWriteMacro cwriter) (toCArray arr) (arrayLen arr)
  const_MAVLINK_STX = 254
  sender :: (SingI n, eff `AllocsIn` s)
         => Uint8 -> ConstRef (Stack s) (Array n (Stored Uint8))
         -> Uint8 -> Ivory eff ()
  sender msgid payload crcextra = do
    seqnum <- getSeqnum
    -- Create header
    header <- local (
      iarray [ ival const_MAVLINK_STX
             , ival (arrayLen payload)
             , ival seqnum
             , ival sysid
             , ival compid
             , ival msgid
             ] :: Init (Array 6 (Stored Uint8)))

    -- Calculate checksum
    (lo, hi) <- mavlinkChecksum (constRef header) payload crcextra
    ckbuf <- local (iarray [ ival lo, ival hi ] :: Init (Array 2 (Stored Uint8)))
    -- write each piece in sequence
    write (constRef header)
    write payload
    write (constRef ckbuf)
  getSeqnum = do
    -- Increment and return sequence number
    seqnum <- addrOf seqnum_area
    s <- deref seqnum
    store seqnum (s + 1)
    return s


