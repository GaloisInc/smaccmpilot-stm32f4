{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Commsec.Ivory.Import.Types where

import Ivory.Language

[ivory|
abstract struct gec_sym_key "gec.h"
abstract struct gec_sts_ctx "gec_ke.h"
abstract struct gec_pubkey  "gec.h"
abstract struct gec_privkey "gec.h"
|]

type RawPublicKey  = Array 32 (Stored Uint8)
type RawPrivateKey = Array 32 (Stored Uint8)
type GecPublicKey     = Struct "gec_pubkey"
type GecPrivateKey    = Struct "gec_privkey"
type GecStsCtx     = Struct "gec_sts_ctx"
type GecSymKey     = Struct "gec_sym_key"
