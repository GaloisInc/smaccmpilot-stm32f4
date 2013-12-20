{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | Shared types and constants shared by SMACCMPILOT and the GCS.

module SMACCMPilot.Communications where

import GHC.TypeLits
import Ivory.Language
import Data.Word

--------------------------------------------------------------------------------

-- Must be smaller than 256 (stored as a Word8 sometimes)
-- SMACCM-SiK restricts Commsec to 96 bytes, so MAVLink must be 16 bytes less
-- than that.

type MAVLinkIx = Ix 80

type MAVLinkArray = Array 80 (Stored Uint8)

mavlinkSize :: Integer
mavlinkSize = fromSing (sing :: Sing 80)

--------------------------------------------------------------------------------
-- Must be mavlinkSize + 16

type CommsecIx = Ix 96

type CommsecArray = Array 96 (Stored Uint8)

commsecPkgSize :: Integer
commsecPkgSize = fromSing (sing :: Sing 96)

--------------------------------------------------------------------------------

-- Must be 2*commsecSize+3.  Must be at greater than 3 (start,stop, and tag
-- bytes).

type HxstreamIx = Ix 195

type HxstreamArray = Array 195 (Stored Uint8)

hxstreamPkgSize :: Integer
hxstreamPkgSize = fromSing (sing :: Sing 195)

--------------------------------------------------------------------------------

-- Tags for HXStream messages.

class Num a => FrameTag a where
  airDataTag   :: a
  airDataTag    = 0
  radioDataTag :: a
  radioDataTag  = 1

instance FrameTag Word8
instance FrameTag Uint8

--------------------------------------------------------------------------------

-- Commsec error reporting, based on commsec.h

class Num a => CommsecError a where
  commsecSuccess         :: a
  commsecSuccess         = 0
  commsecFailBadID       :: a
  commsecFailBadID       = 1
  commsecFailDupCtr      :: a
  commsecFailDupCtr      = 2
  commsecFailCtrRollover :: a
  commsecFailCtrRollover = 3
  -- Yes, we skip 4.
  commsecFailMsgLen      :: a
  commsecFailMsgLen      = 5
  commsecFailGCM         :: a
  commsecFailGCM         = 6

instance CommsecError Word32
instance CommsecError Uint32

showCommsecErrors :: (Show a, Eq a, Num a) => a -> String
showCommsecErrors a = case a of
  0 -> "Successful decrypt"
  1 -> hdr "Out-of-range identifier"
  2 -> hdr "Stale counter"
  3 -> hdr "Counter rollover"
  4 -> hdr "Message is too long"
  5 -> hdr "Bad GCM (IV mismatch)"
  _ -> "Unexpeced commsec error mesage: " ++ show a
  where
  hdr msg = "Failed decrypt!" ++ msg
