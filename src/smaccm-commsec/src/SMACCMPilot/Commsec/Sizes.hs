{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module SMACCMPilot.Commsec.Sizes where

import Ivory.Language
import Ivory.Language.Proxy

-- Note a GHC bug prevents us from using a type alias for 80 and 96 below,
-- morally we should not be repeating ourselves so much.

--------------------------------------------------------------------------------

-- Must be smaller than 256 (stored as a Word8 sometimes)
-- SMACCM-SiK restricts Commsec to 96 bytes, so MAVLink must be 16 bytes less
-- than that.

type PlaintextIx = Ix 80

type PlaintextArray = Array 80 (Stored Uint8)

plaintextSize :: Integer
plaintextSize = fromTypeNat (aNat :: NatType 80)

--------------------------------------------------------------------------------
-- Must be plaintextSize + HEADER_LEN + TAG_LEN == 80 + 4 + 8

type CyphertextIx = Ix 96

type CyphertextArray = Array 96 (Stored Uint8)

cyphertextSize :: Integer
cyphertextSize = fromTypeNat (aNat :: NatType 96)


--------------------------------------------------------------------------------
-- Key Exchange Constants
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Must match gec-key.h! 2*(12+16) = 2*28 = 56
type KeyMaterialIx = Ix 48
type KeyMaterial = Array 48 (Stored Uint8)
keyMaterialSize :: Integer
keyMaterialSize = fromTypeNat (aNat :: NatType 48)

type SymKeySaltIx = Ix 24
type SymKeySaltArray = Array 24 (Stored Uint8)
symKeySaltSize :: Integer
symKeySaltSize = fromTypeNat (aNat :: NatType 24)

type GecKeMessage1Ix = Ix 32
type GecKeMessage1 = Array 32 (Stored Uint8)
gecKeMessage1Size :: Integer
gecKeMessage1Size = fromTypeNat (aNat :: NatType 32)

type GecKeMessage2Ix = Ix 96
type GecKeMessage2 = Array 96 (Stored Uint8)
gecKeMessage2Size :: Integer
gecKeMessage2Size = fromTypeNat (aNat :: NatType 96)

type GecKeMessage3Ix = Ix 64
type GecKeMessage3 = Array 64 (Stored Uint8)
gecKeMessage3Size :: Integer
gecKeMessage3Size = fromTypeNat (aNat :: NatType 64)

type GecKeRandomDataIx = Ix 32
type GecKeRandomData   = Array 32 (Stored Uint8)
gecKeRandomDataSize :: Integer
gecKeRandomDataSize = fromTypeNat (aNat :: NatType 32)
