{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Commsec.Ivory.Import.Types where

import Ivory.Language

-- XXX Also see the Sizes.hs module for related values.

[ivory|
abstract struct gec_sym_key "gec.h"
abstract struct gec_sts_ctx "gec_ke.h"
|]

type KeyArray = Array 16 (Stored Uint8)

