{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
--
-- Param/TowerTypes.hs
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module SMACCMPilot.Param.TowerTypes where

import Ivory.Language
import Ivory.Stdlib.String

-- | Generated function to read a parameter's value.
type ParamGetter = ProcPtr ('[]       :-> IFloat)

-- | Generated function to set a parameter's value.
type ParamSetter = ProcPtr ('[IFloat] :-> ())

-- | Define a string type for parameter names.
[ivory|
string ParamString 16
|]

