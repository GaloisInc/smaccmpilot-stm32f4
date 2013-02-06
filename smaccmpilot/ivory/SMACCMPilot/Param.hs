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

-- | The maximum number of parameters supported at run-time.
type MaxParams = 512

-- | Index type for the parameters array.
type ParamCount = Ix Uint16 MaxParams

-- | Run-time information about a parameter.
[ivory|
 struct param_info
 { param_type      :: Stored Uint8
 ; param_name      :: Array 32 (Stored IChar)
 ; param_ptr_u8    :: Stored (Ptr Global (Stored Uint8))
 ; param_ptr_u16   :: Stored (Ptr Global (Stored Uint16))
 ; param_ptr_u32   :: Stored (Ptr Global (Stored Uint32))
 ; param_ptr_float :: Stored (Ptr Global (Stored IFloat))
 }
|]

-- | Global array of "param_info" structures containing all known
-- parameters.  This is filled in by calling "param_init".
param_info :: MemArea (Array MaxParams (Struct "param_info"))
param_info = area "g_param_info" Nothing

-- | Global containing the number of entries in "param_info".
param_count :: MemArea (Stored ParamCount)
param_count = area "g_param_count" Nothing

-- FIXME: This should be defined in "ivory-language" somewhere.
instance (SingI len, IvoryIx rep, IvoryStore lex s rep) =>
    IvoryStore lex s (Ix rep len)

-- | Increment "param_count" and return an entry to be filled in when
-- a new parameter is added.
new_param :: Def ('[] :-> Ref Global (Struct "param_info"))
new_param = proc "new_param" $ body $ do
  count_ref <- addrOf param_count
  count     <- deref count_ref
  store count_ref (count + 1)

  info  <- addrOf param_info
  entry <- assign (info ! count)
  ret entry

-- | Initialize a parameter of type "Uint8".
init_param_u8 :: Def ('[ IString                    -- name
                       , Ref Global (Stored Uint8)] -- ref
                      :-> ())
init_param_u8 = proc "init_param_u8" $ \name ref -> body $ do
  entry <- call new_param
  strcpy (entry ~> param_name)   name
  store  (entry ~> param_type)   paramTypeU8
  store  (entry ~> param_ptr_u8) (refToPtr ref)

-- | Initialize a parameter of type "Uint16".
init_param_u16 :: Def ('[ IString                     -- name
                        , Ref Global (Stored Uint16)] -- ref
                       :-> ())
init_param_u16 = proc "init_param_u16" $ \name ref -> body $ do
  entry <- call new_param
  strcpy (entry ~> param_name)    name
  store  (entry ~> param_type)    paramTypeU16
  store  (entry ~> param_ptr_u16) (refToPtr ref)

-- | Initialize a parameter of type "Uint32".
init_param_u32 :: Def ('[ IString                     -- name
                        , Ref Global (Stored Uint32)] -- ref
                       :-> ())
init_param_u32 = proc "init_param_u32" $ \name ref -> body $ do
  entry <- call new_param
  strcpy (entry ~> param_name)    name
  store  (entry ~> param_type)    paramTypeU32
  store  (entry ~> param_ptr_u32) (refToPtr ref)

-- | Initialize a parameter of type "IFloat".
init_param_float :: Def ('[ IString                     -- name
                          , Ref Global (Stored IFloat)] -- ref
                         :-> ())
init_param_float = proc "init_param_float" $ \name ref -> body $ do
  entry <- call new_param
  strcpy (entry ~> param_name)      name
  store  (entry ~> param_type)      paramTypeFloat
  store  (entry ~> param_ptr_float) (refToPtr ref)

-- | Return the number of defined parameters.
get_param_count :: Ivory s r ParamCount
get_param_count = deref =<< addrOf param_count

-- | Look up a parameter by name and retrieve its "param_info" entry
-- if it exists.  Returns "nullPtr" if no parameter with that name is
-- found.
get_param_by_name :: Def ('[ConstRef s (CArray (Stored IChar))]
                          :-> Ptr Global (Struct "param_info"))
get_param_by_name = proc "get_param_by_name" $ \name -> body $ do
  count <- get_param_count
  info  <- addrOf param_info

  count `times` \ix -> do
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
get_param_by_index :: Def ('[ParamCount]
                           :-> Ptr Global (Struct "param_info"))
get_param_by_index = proc "get_param_by_index" $ \ix -> body $ do
  count <- get_param_count
  ift (ix >=? count)
    (ret nullPtr)

  info <- addrOf param_info
  ret (refToPtr (info ! ix))

----------------------------------------------------------------------
-- Parameter Initialization

-- | Type class used to select the appropriate "init_param_XXX"
-- function based on the type of the value reference.
--
-- Modules can define instances of "init_param" to work on structures
-- and initialize multiple fields at once by appending suffixes to the
-- strings.
class ParamType a where
  init_param :: String -> Ref Global a -> Ivory s r ()

instance ParamType (Stored Uint8) where
  init_param name ref = call_ init_param_u8 (fromString name) ref

instance ParamType (Stored Uint16) where
  init_param name ref = call_ init_param_u16 (fromString name) ref

instance ParamType (Stored Uint32) where
  init_param name ref = call_ init_param_u32 (fromString name) ref

instance ParamType (Stored IFloat) where
  init_param name ref = call_ init_param_float (fromString name) ref

-- | Initialize a parameter with value stored in a "MemArea".
init_param_area :: (ParamType a, IvoryType a) =>
                   String -> MemArea a -> Ivory s r ()
init_param_area name a = init_param name =<< addrOf a

----------------------------------------------------------------------
-- Ivory Module

paramModule :: Module
paramModule = package "param" $ do
  depend cstringModule
  defStruct (Proxy :: Proxy "param_info")
  defMemArea param_info
  defMemArea param_count
  incl new_param
  incl init_param_u8
  incl init_param_u16
  incl init_param_u32
  incl init_param_float
  incl get_param_by_name
  incl get_param_by_index
