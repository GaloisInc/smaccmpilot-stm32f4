{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Commsec.Ivory.Import.Types where

import Ivory.Language
import Ivory.Language.Proxy

-- XXX Also see the Sizes.hs module for related values.

[ivory|
abstract struct gec_sym_key "gec.h"
abstract struct gec_sts_ctx "gec_ke.h"
struct gec_pubkey {
            pub :: Array 32 (Stored Uint8)
            }
struct gec_privkey {
            priv :: Array 32 (Stored Uint8)
            ; pub_ :: Array 32 (Stored Uint8)
            }
|]


-- XXX Brittle code above assumes a layout to the underlying C structure.
-- It would be better to use the below and a C translation function from
-- Array to *Key structs that can be maintained side-by-side with the
-- structure itself.
--
-- abstract struct gec_pubkey "gec.h"
-- abstract struct gec_privkey "gec.h"

type KeyArray = Array 16 (Stored Uint8)

-- type PublicKeyIx = Ix 32
type PublicKey = Struct "gec_pubkey" -- Array 32 (Stored Uint8)
publicKeySize :: Integer
publicKeySize = fromTypeNat (aNat :: NatType 32)

-- type PrivateKeyIx = Ix 32
type PrivateKey = Struct "gec_privkey" -- Array 32 (Stored Uint8)
privateKeySize :: Integer
privateKeySize = fromTypeNat (aNat :: NatType 32)

