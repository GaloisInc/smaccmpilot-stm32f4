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
-- Must be plaintextSize + HEADER_LEN + TAG_LEN == 80 + 4 + 16

type CyphertextIx = Ix 100

type CyphertextArray = Array 100 (Stored Uint8)

cyphertextSize :: Integer
cyphertextSize = fromTypeNat (aNat :: NatType 100)

