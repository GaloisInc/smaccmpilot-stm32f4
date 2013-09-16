{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
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

type SenderMacro cs s n =  Uint8 -- id
                        -> ConstRef s (Array n (Stored Uint8)) -- buf
                        -> Uint8 -- crcextra
                        -> Ivory (AllocEffects cs) ()

data SizedMavlinkSender n =
  SizedMavlinkSender
    { senderMacro :: forall s cs . SenderMacro cs s n
    , senderName  :: String
    , senderDeps  :: ModuleDef
    }

newtype MavlinkSender =
  MavlinkSender (forall n . (SingI n) =>  SizedMavlinkSender n)

mavlinkSenderName :: MavlinkSender -> String
mavlinkSenderName (MavlinkSender sized) =
  senderName (sized :: SizedMavlinkSender 1)

class MavlinkSendable t n | t -> n where
  mkSender :: SizedMavlinkSender n -> Def ('[ ConstRef s (Struct t) ] :-> ())

newtype MavlinkWriteMacro = MavlinkWriteMacro {
  unMavlinkWriteMacro ::
    (forall s cs n
    . (SingI n)
    => ConstRef s (Array n (Stored Uint8)) -- buf
    -> Ivory (AllocEffects cs) ()) }

mavlinkChecksum :: (SingI n, GetAlloc eff ~ Scope s)
                             => ConstRef s1 (Array 6 (Stored Uint8))
                             -> ConstRef s2 (Array n (Stored Uint8))
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
                      -> Ref s (Stored Uint8) -- TX Sequence Number
                      -> MavlinkWriteMacro -- Ivory macro: send bytes on chan
                      -> ModuleDef -- Deps of tx seq mem area, ivory macro
                      -> MavlinkSender
mavlinkSendWithWriter sysid compid name seqnum cwriter writerdeps =
  MavlinkSender (SizedMavlinkSender sender name deps)
  where
  deps = do
    depend mavlinkCRCModule
    writerdeps
  write :: (SingI n)
        => ConstRef s (Array n (Stored Uint8)) -> Ivory (AllocEffects cs) ()
  write = unMavlinkWriteMacro cwriter

  const_MAVLINK_STX = 254 :: Uint8

  sender :: (SingI n)
         => Uint8 -> ConstRef s' (Array n (Stored Uint8))
         -> Uint8 -> Ivory (AllocEffects cs) ()
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
    ckbuf    <- local
                  (iarray [ ival lo, ival hi ] :: Init (Array 2 (Stored Uint8)))
    -- write each piece in sequence
    write (constRef header)
    write payload
    write (constRef ckbuf)
    where
    getSeqnum = do
      -- Increment and return sequence number
      s <- deref seqnum
      store seqnum (s + 1)
      return s


