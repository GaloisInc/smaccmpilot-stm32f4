{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Types where

import Ivory.Language

--------------------------------------------------------------------------------

-- | Struct sent by plugin function (record_assingment()) over the channel.
[ivory|

struct assignment
  { var_id :: Stored (Ix 100)
  ; value  :: Stored Uint32
  }

|]

--------------------------------------------------------------------------------

type AssignStruct = Struct "assignment"
type AssignRef s = ConstRef s AssignStruct

--------------------------------------------------------------------------------

