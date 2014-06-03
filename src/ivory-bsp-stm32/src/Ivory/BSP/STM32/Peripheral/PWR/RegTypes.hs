{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
--
-- RegTypes.hs --- Types for register fields in PWR driver
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.PWR.RegTypes where

import Ivory.Language

[ivory|

 bitdata PWR_PVD :: Bits 3
 = pwr_pvd_2_0v      as 0
 | pwr_pvd_2_1v      as 1
 | pwr_pvd_2_3v      as 2
 | pwr_pvd_2_5v      as 3
 | pwr_pvd_2_6v      as 4
 | pwr_pvd_2_7v      as 5
 | pwr_pvd_2_8v      as 6
 | pwr_pvd_2_9v      as 7

|]
