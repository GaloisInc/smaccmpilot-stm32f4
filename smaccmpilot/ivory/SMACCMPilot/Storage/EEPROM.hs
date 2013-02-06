{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
--
-- EEPROM.hs --- SMACCMPilot EEPROM driver.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module SMACCMPilot.Storage.EEPROM (
  eeprom_init,
  eeprom_read_byte, eeprom_write_byte,
  eeprom_read, eeprom_write,
  eepromModule
) where

import Ivory.Language

import SMACCMPilot.Driver.I2C

----------------------------------------------------------------------
-- HWF4 Bindings

-- | Initialize the EEPROM driver given its I2C bus and address.
eeprom_init :: Def ('[Ref s (Struct "i2cdrv_t"), Uint8] :-> ())
eeprom_init = importProc "eeprom_init" "hwf4/eeprom"

-- | Read a single byte from the EEPROM at an address.
eeprom_read_byte :: Def ('[Uint16, Ref s (Stored Uint8)] :-> IBool)
eeprom_read_byte = importProc "eeprom_read_byte" "hwf4/eeprom"

-- | Write a single byte to the EEPROM at an address.
eeprom_write_byte :: Def ('[Uint16, Uint8] :-> IBool)
eeprom_write_byte = importProc "eeprom_write_byte" "hwf4/eeprom"

-- | Read multiple bytes from the EEPROM into a byte array.
eeprom_read :: Def ('[ Uint16                        -- addr
                     , Ref s (CArray (Stored Uint8)) -- buf
                     , Uint32]                       -- len
                    :-> IBool)
eeprom_read = importProc "eeprom_read" "hwf4/eeprom"

-- | Write multiple bytes to the EEPROM from a byte array.
eeprom_write :: Def ('[ Uint16                             -- addr
                      , ConstRef s (CArray (Stored Uint8)) -- buf
                      , Uint32]                            -- len
                     :-> IBool)
eeprom_write = importProc "eeprom_write" "hwf4/eeprom"

{-
----------------------------------------------------------------------
-- Test Code

-- | Test initializing, reading, and writing a byte.
eeprom_test1 :: Def ('[] :-> Uint8)
eeprom_test1 = proc "eeprom_test1" $ body $ do
  drv <- addrOf i2c2
  call_ eeprom_init drv 0x50
  call_ eeprom_write_byte 0 0xCC
  byte <- local (ival 0)
  call_ eeprom_read_byte 0 byte
  ret =<< deref byte

-- | Test reading and writing buffers.
eeprom_test2 :: Def ('[] :-> ())
eeprom_test2 = proc "eeprom_test2" $ body $ do
  drv <- addrOf i2c2
  call_ eeprom_init drv 0x50
  (buf :: Ref (Stack s) (Array 128 (Stored Uint8))) <- local (iarray [])
  arrayMap $ \i -> store (buf ! i) (fromIx i)
  call_ eeprom_write 0 (constRef (toCArray buf)) (arrayLen buf)
  arrayMap $ \i -> store (buf ! (i :: Ix Uint32 128)) 0
  call_ eeprom_read 0 (toCArray buf) (arrayLen buf)
-}

----------------------------------------------------------------------
-- Ivory Module

eepromModule :: Module
eepromModule = package "storage_eeprom" $ do
  depend i2cModule
  incl eeprom_init
  incl eeprom_read_byte
  incl eeprom_write_byte
  incl eeprom_read
  incl eeprom_write
  -- incl eeprom_test1
  -- incl eeprom_test2
