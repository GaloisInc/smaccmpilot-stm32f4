{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
--
-- IvoryCString.hs --- C-string utilities for Ivory.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module IvoryCString (
  copy_istring,
  strcpy,
  strncmp,
  cstringModule
) where

import Ivory.Language

-- | Safely copy an IString (string literal) into a character array.
-- The resulting string will always be null terminated.
copy_istring :: Def ('[ Ref s (CArray (Stored IChar)) -- dest
                      , IString                       -- src
                      , Uint32]                       -- len
                     :-> ())
copy_istring = importProc "ivory_strlcpy" "smaccmpilot/ivory_string_prim"

-- | Type class to generate the correct call to a string function to
-- copy one C string to another.
class (IvoryType dest, IvoryType src) => Strcpy dest src where
  strcpy :: dest -> src -> Ivory s r ()

-- | Strcpy instance for copying string constants to arrays of
-- characters.
instance (SingI len) => Strcpy (Ref s (Array len (Stored IChar))) IString where
  strcpy dest src = call_ copy_istring (toCArray dest) src (arrayLen dest)

-- | Binding to the C "strncmp" function.
strncmp :: Def ('[ ConstRef s1 (CArray (Stored IChar)) -- s1
                 , ConstRef s2 (CArray (Stored IChar)) -- s2
                 , Uint32]                             -- len
                :-> Sint32)
strncmp = importProc "strncmp" "string"

-- | Ivory module definition.
cstringModule :: Module
cstringModule = package "ivory_string" $ do
  inclHeader "smaccmpilot/ivory_string_prim"
  inclHeader "string"
  incl copy_istring
  incl strncmp
