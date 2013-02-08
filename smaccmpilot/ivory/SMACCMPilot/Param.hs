{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--
-- Param.hs --- MAVLink parameter support.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module SMACCMPilot.Param where

import Data.String
import Ivory.Language

import IvoryHelpers
import IvoryCString

--
-- Design notes:
--
-- * Parameters are built around Ivory references.  Each module
--   registers its parameters by calling "param_init" with the
--   parameter name and a reference to its value.
--
-- * The run time table contains the name, type code, and reference to
--   the parameter's value.  This is used to save/load parameters from
--   storage and get/set them over MAVLink.
--

----------------------------------------------------------------------
-- Run-Time Parameter Info

-- | Type codes for parameter values.
paramTypeU8, paramTypeU16, paramTypeU32, paramTypeFloat :: Uint8
paramTypeU8    = 0
paramTypeU16   = 1
paramTypeU32   = 2
paramTypeFloat = 3

-- | Run-time information about a parameter.
[ivory|
 struct param_info
 { param_type      :: Stored Uint8
 ; param_name      :: Array 32 (Stored IChar)
 ; param_index     :: Stored (Ix Uint16 512) -- argh
 ; param_ptr_u8    :: Stored (Ptr Global (Stored Uint8))
 ; param_ptr_u16   :: Stored (Ptr Global (Stored Uint16))
 ; param_ptr_u32   :: Stored (Ptr Global (Stored Uint32))
 ; param_ptr_float :: Stored (Ptr Global (Stored IFloat))
 ; param_requested :: Stored Uint8
 }
|]

-- | Global array of "param_info" structures containing all known
-- parameters.  This is filled in by calling "param_init".
param_info :: MemArea (Array 512 (Struct "param_info"))
param_info = area "g_param_info" Nothing

-- | Global containing the number of entries in "param_info".
param_count :: MemArea (Stored (Ix Uint16 512))
param_count = area "g_param_count" Nothing

-- FIXME: This should be defined in "ivory-language" somewhere.
instance (SingI len, IvoryIx rep, IvoryStore lex s rep) =>
    IvoryStore lex s (Ix rep len)

-- | Increment "param_count" and return an entry to be filled in when
-- a new parameter is added.
param_new :: Def ('[] :-> Ref Global (Struct "param_info"))
param_new = proc "param_new" $ body $ do
  count_ref <- addrOf param_count
  count     <- deref count_ref
  store count_ref (count + 1)

  info  <- addrOf param_info
  entry <- assign (info ! count)
  store (entry ~> param_index) count
  store (entry ~> param_requested) 0
  ret entry

-- | Initialize a parameter of type "Uint8".
param_init_u8 :: Def ('[ IString                    -- name
                       , Ref Global (Stored Uint8)] -- ref
                      :-> ())
param_init_u8 = proc "param_init_u8" $ \name ref -> body $ do
  entry <- call param_new
  strcpy (entry ~> param_name)   name
  store  (entry ~> param_type)   paramTypeU8
  store  (entry ~> param_ptr_u8) (refToPtr ref)

-- | Initialize a parameter of type "Uint16".
param_init_u16 :: Def ('[ IString                     -- name
                        , Ref Global (Stored Uint16)] -- ref
                       :-> ())
param_init_u16 = proc "param_init_u16" $ \name ref -> body $ do
  entry <- call param_new
  strcpy (entry ~> param_name)    name
  store  (entry ~> param_type)    paramTypeU16
  store  (entry ~> param_ptr_u16) (refToPtr ref)

-- | Initialize a parameter of type "Uint32".
param_init_u32 :: Def ('[ IString                     -- name
                        , Ref Global (Stored Uint32)] -- ref
                       :-> ())
param_init_u32 = proc "param_init_u32" $ \name ref -> body $ do
  entry <- call param_new
  strcpy (entry ~> param_name)    name
  store  (entry ~> param_type)    paramTypeU32
  store  (entry ~> param_ptr_u32) (refToPtr ref)

-- | Initialize a parameter of type "IFloat".
param_init_float :: Def ('[ IString                     -- name
                          , Ref Global (Stored IFloat)] -- ref
                         :-> ())
param_init_float = proc "param_init_float" $ \name ref -> body $ do
  entry <- call param_new
  strcpy (entry ~> param_name)      name
  store  (entry ~> param_type)      paramTypeFloat
  store  (entry ~> param_ptr_float) (refToPtr ref)

-- | Return the number of defined parameters.
param_get_count :: Ivory s r (Ix Uint16 512)
param_get_count = deref =<< addrOf param_count

-- | Look up a parameter by name and retrieve its "param_info" entry
-- if it exists.  Returns "nullPtr" if no parameter with that name is
-- found.
param_get_by_name :: Def ('[ConstRef s (CArray (Stored IChar))]
                          :-> Ptr Global (Struct "param_info"))
param_get_by_name = proc "param_get_by_name" $ \name -> body $ do
  count <- param_get_count
  ift (count ==? 0)
    (ret nullPtr)

  info  <- addrOf param_info
  upTo 0 (count - 1) $ \ix -> do
    entry <- assign (info ! ix)
    name' <- assign (constRef (toCArray (entry ~> param_name)))
    len   <- assign (arrayLen (entry ~> param_name))
    r     <- call strncmp name name' len

    ift (r ==? 0)
      (ret (refToPtr entry))

  ret nullPtr

-- | Look up a parameter by index and retrieve its "param_info" entry
-- if it exists.  Returns "nullPtr" if no parameter with that index is
-- defined.
param_get_by_index :: Def ('[Ix Uint16 512]
                           :-> Ptr Global (Struct "param_info"))
param_get_by_index = proc "param_get_by_index" $ \ix -> body $ do
  count <- param_get_count
  ift (ix >=? count)
    (ret nullPtr)

  info <- addrOf param_info
  ret (refToPtr (info ! ix))

-- | Return the first requested parameter, or NULL if no parameters
-- are marked for transmission.
param_get_requested :: Def ('[] :-> Ptr Global (Struct "param_info"))
param_get_requested = proc "param_get_requested" $ body $ do
  count <- param_get_count
  ift (count ==? 0)
    (ret nullPtr)

  info  <- addrOf param_info
  upTo 0 (count - 1) $ \ix -> do
    entry <- assign (info ! ix)
    flag  <- deref (entry ~> param_requested)

    ift (flag /=? 0)
      (ret (refToPtr entry))

  ret nullPtr

-- | Extract the value of a parameter, casted to an IFloat.
param_get_float_value :: Def ('[Ref s (Struct "param_info")] :-> IFloat)
param_get_float_value = proc "param_get_float_value" $ \info -> body $ do
  type_code <- deref (info ~> param_type)

  -- ick...
  ift (type_code ==? paramTypeU8)
    (do ptr <- deref (info ~> param_ptr_u8)
        withRef ptr (\x -> do val <- deref x
                              ret (toFloat val)) (ret 0.0))

  ift (type_code ==? paramTypeU16)
    (do ptr <- deref (info ~> param_ptr_u16)
        withRef ptr (\x -> do val <- deref x
                              ret (toFloat val)) (ret 0.0))

  ift (type_code ==? paramTypeU32)
    (do ptr <- deref (info ~> param_ptr_u32)
        withRef ptr (\x -> do val <- deref x
                              ret (toFloat val)) (ret 0.0))

  ift (type_code ==? paramTypeFloat)
    (do ptr <- deref (info ~> param_ptr_float)
        withRef ptr (\x -> do val <- deref x
                              ret val) (ret 0.0))

  ret 0.0

----------------------------------------------------------------------
-- Parameter Initialization

-- | Type class used to select the appropriate "param_init_XXX"
-- function based on the type of the value reference.
--
-- Modules can define instances of "param_init" to work on structures
-- and initialize multiple fields at once by appending suffixes to the
-- strings.
class ParamType a where
  param_init :: String -> Ref Global a -> Ivory s r ()

instance ParamType (Stored Uint8) where
  param_init name ref = call_ param_init_u8 (fromString name) ref

instance ParamType (Stored Uint16) where
  param_init name ref = call_ param_init_u16 (fromString name) ref

instance ParamType (Stored Uint32) where
  param_init name ref = call_ param_init_u32 (fromString name) ref

instance ParamType (Stored IFloat) where
  param_init name ref = call_ param_init_float (fromString name) ref

-- | Initialize a parameter with value stored in a "MemArea".
param_init_area :: (ParamType a, IvoryType a) =>
                   String -> MemArea a -> Ivory s r ()
param_init_area name a = param_init name =<< addrOf a

----------------------------------------------------------------------
-- Ivory Module

paramModule :: Module
paramModule = package "param" $ do
  depend cstringModule
  defStruct (Proxy :: Proxy "param_info")
  defMemArea param_info
  defMemArea param_count
  incl param_new
  incl param_init_u8
  incl param_init_u16
  incl param_init_u32
  incl param_init_float
  incl param_get_by_name
  incl param_get_by_index
  incl param_get_requested
  incl param_get_float_value
