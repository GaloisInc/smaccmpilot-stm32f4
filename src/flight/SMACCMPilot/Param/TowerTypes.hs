{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
--
-- Param/TowerTypes.hs
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module SMACCMPilot.Param.TowerTypes where

import Ivory.Language

-- | Generated function to read a parameter's value.
type ParamGetter = ProcPtr ('[]       :-> IFloat)

-- | Generated function to set a parameter's value.
type ParamSetter = ProcPtr ('[IFloat] :-> ())
