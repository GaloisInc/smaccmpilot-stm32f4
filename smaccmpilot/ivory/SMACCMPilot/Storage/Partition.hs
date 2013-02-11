{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
--
-- Partition.hs --- Simple EEPROM partitioning.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module SMACCMPilot.Storage.Partition where

import Ivory.Language

import IvoryHelpers
import SMACCMPilot.Storage.EEPROM

----------------------------------------------------------------------
-- Partition Table

-- | Structure containing the size and bounds of a partition.
[ivory|
 struct partition_info
 { partition_info_id    :: (Stored Uint8)
 ; partition_info_size  :: (Stored Uint16)
 ; partition_info_start :: (Stored Uint16)
 }
|]

-- | Partition data, as a constant global array.
partition_table :: ConstMemArea (Array 3 (Struct "partition_info"))
partition_table = constArea "g_partition_table" $ iarray
  [ istruct
     [ partition_info_id    .= ival 0
     , partition_info_size  .= ival 0
     , partition_info_start .= ival 0]
  , istruct
     [ partition_info_id    .= ival 1
     , partition_info_size  .= ival 0x1000
     , partition_info_start .= ival 0x0000]
  , istruct
     [ partition_info_id    .= ival 2
     , partition_info_size  .= ival 0x1000
     , partition_info_start .= ival 0x1000]
  ]

----------------------------------------------------------------------
-- Ivory Module

partitionModule :: Module
partitionModule = package "storage_partition" $ do
  depend eepromModule
