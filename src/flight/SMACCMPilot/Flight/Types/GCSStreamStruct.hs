{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Flight.Types.GCSStreamStruct where

import Ivory.Language
import Ivory.Tower.Types.Time

[ivory|

struct gcsstream_data
  { period        :: Stored ITime
  ; hard_deadline :: Stored IBool -- hard real-time or soft real-time
  }

|]

