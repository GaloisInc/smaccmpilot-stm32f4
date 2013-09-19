{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Mavlink.Send where

import qualified Control.Monad as M

import Ivory.Language
import Ivory.Stdlib

import SMACCMPilot.Mavlink.CRC

--------------------------------------------------------------------------------

-- type SenderMacro cs s n =  Uint8 -- id
--                         -> ConstRef s (Array n (Stored Uint8)) -- buf
--                         -> Uint8 -- crcextra
--                         -> Ivory (AllocEffects cs) ()

-- data SizedMavlinkSender n =
--   SizedMavlinkSender
--     { senderMacro :: forall s cs . SenderMacro cs s n
--     , senderName  :: String
--     , senderDeps  :: ModuleDef
--     }

-- class MavlinkSendable t n | t -> n where
--   mkSender :: SizedMavlinkSender n -> Def ('[ ConstRef s (Struct t) ] :-> ())

-- mavlinkChecksum :: Uint8
--                 -> Uint8
--                 -> Ref s (Array 128 (Stored Uint8))
--                 -> Ivory eff ()
mavlinkChecksum ::
     (GetAlloc eff ~ Scope cs)
  => Uint8
  -> Uint8
  -> Ref s (Array 128 (Stored Uint8))
  -> Ivory eff ()
mavlinkChecksum sz crcextra arr = do
  ck <- local (ival crc_init_v)
  for (toIx sz) $ \i ->
    -- mavlink doesn't use the magic number
    -- in header[0] for crc calculation.
    unless (i ==? 0) $ do
      b <- deref (arr ! i)
      call_ crc_accumulate b ck

  call_ crc_accumulate crcextra ck
  (lo, hi) <- crc_lo_hi ck

  assert (arrayLen arr >? (sz + 1))
  let szIx = toIx sz
  store (arr ! szIx) lo
  store (arr ! (szIx + 1)) hi

-- Magic constants
sysid, compid :: Uint8
sysid  = 1
compid = 0

const_MAVLINK_STX :: Uint8
const_MAVLINK_STX = 254

-- We assume the payload has already been copied into arr.
mavlinkSendWithWriter ::
  Def ('[ Uint8
        , Uint8
        , Uint8
        , Ref s (Stored Uint8)
        , Ref s (Array 128 (Stored Uint8))
        ]
        :-> ()
      )
mavlinkSendWithWriter =
--  (SizedMavlinkSender sender (writerName mavlinkData) deps)
  proc "mavlinkSendWithWriter"
  $ \msgId crcExtra payloadLen seqNum arr -> body
  $ do

    s <- deref seqNum
    header <- local (
      iarray [ ival const_MAVLINK_STX
             , ival payloadLen
             , ival s
             , ival sysid
             , ival compid
             , ival msgId
             ] :: Init (Array 6 (Stored Uint8)))

    seqNum += 1
    let sz = 6 + payloadLen :: Uint8

    M.void (arrCopy arr header 0)

    -- Calculate checksum and store in arr
    mavlinkChecksum sz crcExtra arr

mavlinkSendModule :: Module
mavlinkSendModule = package "mavlinkSendModule" $
  incl mavlinkSendWithWriter

--------------------------------------------------------------------------------
-- Helpers for auto-generated code, also used by transmitter in SMACCMPILOT
-- currently.

-- XXX refactor and get rid of this stuff at some point.

-- newtype MavlinkSender =
--   MavlinkSender (forall n . (SingI n) => SizedMavlinkSender n)

-- mkMessage :: (forall n . (SingI n) => SizedMavlinkSender n) -> MavlinkSender
-- mkMessage = MavlinkSender
