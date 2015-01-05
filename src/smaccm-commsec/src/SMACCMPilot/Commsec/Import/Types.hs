{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Commsec.Import.Types where

import Ivory.Language

[ivory|
abstract struct commsec_encode "commsec.h"
abstract struct commsec_decode "commsec.h"
|]

type KeyArray = Array 16 (Stored Uint8)

