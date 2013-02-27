{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
--
-- Console.hs --- SMACCMPilot debug console output.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module SMACCMPilot.Console where

import Data.String
import Ivory.Language

-- | Class of types that are printable on the console.  Each type is
-- associated with a primitive function defined in C that knows
-- how to print a value of that type.
--
-- It should be possible to define custom printers for structure types
-- as well (test this with PID).
class (IvoryExpr a) => Writable a where
  write :: (IvoryType r) => a -> Ivory s r ()

-- TODO: Add a way to write multiple objects in a single call?

-- | Special case for writing a string literal to get around ambiguity
-- with overloaded strings.
writes :: (IvoryType r) => String -> Ivory s r ()
writes s = write (fromString s :: IString)

-- | Write a reference to a writable object.
instance (Writable a) => Writable (Ref s (Stored a)) where
  write x = write =<< deref x

instance (Writable a) => Writable (ConstRef s (Stored a)) where
  write x = write =<< deref x

-- | Write an array of characters as a string.  We don't assume the
-- string is null-terminated, but will stop writing if it is.
instance (SingI len) => Writable (Ref s (Array len (Stored IChar))) where
  write x = call_ write_string_n (constRef (toCArray x)) (arrayLen x)

----------------------------------------------------------------------
-- Primitive C Printers

write_u8 :: Def ('[Uint8] :-> ())
write_u8 = importProc "console_write_u8" "smaccmpilot/console_prim"

instance Writable Uint8 where
  write = call_ write_u8

write_s8 :: Def ('[Sint8] :-> ())
write_s8 = importProc "console_write_s8" "smaccmpilot/console_prim"

instance Writable Sint8 where
  write = call_ write_s8

write_u16 :: Def ('[Uint16] :-> ())
write_u16 = importProc "console_write_u16" "smaccmpilot/console_prim"

instance Writable Uint16 where
  write = call_ write_u16

write_s16 :: Def ('[Sint16] :-> ())
write_s16 = importProc "console_write_s16" "smaccmpilot/console_prim"

instance Writable Sint16 where
  write = call_ write_s16

write_u32 :: Def ('[Uint32] :-> ())
write_u32 = importProc "console_write_u32" "smaccmpilot/console_prim"

instance Writable Uint32 where
  write = call_ write_u32

write_s32 :: Def ('[Sint32] :-> ())
write_s32 = importProc "console_write_s32" "smaccmpilot/console_prim"

instance Writable Sint32 where
  write = call_ write_s32

instance (SingI len) => Writable (Ix len) where
  write x = call_ write_s32 (ixCast x)

write_float :: Def ('[IFloat] :-> ())
write_float = importProc "console_write_float" "smaccmpilot/console_prim"

instance Writable IFloat where
  write = call_ write_float

write_double :: Def ('[IDouble] :-> ())
write_double = importProc "console_write_double" "smaccmpilot/console_prim"

instance Writable IDouble where
  write = call_ write_double

write_istring :: Def ('[IString] :-> ())
write_istring = importProc "console_write_string" "smaccmpilot/console_prim"

instance Writable IString where
  write = call_ write_istring

write_string_n :: Def ('[ConstRef s (CArray (Stored IChar)), Sint32] :-> ())
write_string_n = importProc "console_write_string_n" "smaccmpilot/console_prim"

----------------------------------------------------------------------
-- Ivory Module

consoleModule :: Module
consoleModule = package "console" $ do
  inclHeader "smaccmpilot/console_prim"
  incl write_u8
  incl write_s8
  incl write_u16
  incl write_s16
  incl write_u32
  incl write_s32
  incl write_float
  incl write_double
  incl write_istring
  incl write_string_n
