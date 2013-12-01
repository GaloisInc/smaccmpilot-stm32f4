{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
--
-- Param/Tower.hs --- Parameter storage using Tower.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module SMACCMPilot.Param.Tower where

import Data.Traversable (Traversable, traverse)

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib (when)
import Ivory.Stdlib.String

import SMACCMPilot.Param.Base
import SMACCMPilot.Param.TowerTypes

import qualified SMACCMPilot.Mavlink.Messages.ParamValue as PV

----------------------------------------------------------------------
-- Tower Parameters

-- | Shorthand type for a data source of parameter values.
type ParamSource = DataSource (Stored IFloat)

-- | Shorthand type for a data sink of parameter values.
type ParamSink = DataSink (Stored IFloat)

-- | Wrapper for a source/sink dataport pair.
data PortPair = PortPair
  { portPairSource :: ParamSource
  , portPairSink   :: ParamSink
  }

-- | Initialize a parameter with a Tower data port pair.
initTowerParams :: ParamT PortPair (Tower p) (a PortPair)
                -> Tower p (a PortPair, [Param PortPair])
initTowerParams x = paramInit go x
  where
    go :: String -> Float -> Tower p PortPair
    go _ v = do
      (psrc, psink) <- dataportInit (ival (ifloat v))
      return (PortPair psrc psink)

-- | Shorthand type for a reader for parameter values.
type ParamReader = DataReader (Stored IFloat)

-- | Shorthand type for a writer for parameter values.
type ParamWriter = DataWriter (Stored IFloat)

-- | Open data readers for a parameter tree containing sinks.
paramReader :: (DataPortable i, Traversable a)
            => a ParamSink
            -> Node i p (a ParamReader)
paramReader = traverse (\x -> withDataReader x "paramReader")

-- | Open data writers for a parameter tree containing sources.
paramWriter :: (DataPortable i, Traversable a)
            => a ParamSource
            -> Node i p (a ParamWriter)
paramWriter = traverse (\x -> withDataWriter x "paramWriter")

-- | Read the float values of a parameter tree.
paramRead :: (GetAlloc eff ~ Scope s, Traversable a)
          => a ParamReader
          -> Ivory eff (a IFloat)
paramRead =
  traverse $ \d -> do
    x <- local (ival 0.0)
    readData d x
    deref x

-- | Shorthand for reading a single unwrapped Param value
paramGet :: (GetAlloc eff ~ Scope s)
         => Param ParamReader -> Ivory eff IFloat
paramGet r = do
  p <- paramRead r
  return (paramData p)

----------------------------------------------------------------------
-- Parameter Lookup

-- | Run-time information about a parameter.
[ivory|
 struct param_info
 { param_name      :: ParamString -- Struct "param_string"
 ; param_index     :: Stored Sint32
 ; param_value     :: Stored IFloat
 }
|]

-- Rank N type wrapping up the ParamIndexGetter callback
-- to ensure it is always polymorphic on the ref scopes
newtype ParamIndexGetter =
  ParamIndexGetter
    { paramIndexGetter :: forall s1 s2 .
        Def ('[ ConstRef s1 ParamString
              , Ref      s2 (Stored Sint16)
        ] :-> IBool)
    }

-- | Create a function to look up a parameter index by name.
makeGetParamIndex :: [Param f]
                  -> Task p ParamIndexGetter
makeGetParamIndex params = do
  fr <- fresh
  let p = ParamIndexGetter $ proc ("getParamIndex" ++ (show fr)) $
            \name ref -> body $ go params name ref 0
  taskModuleDef $ incl (paramIndexGetter p)
  return p
  where
    go [] _ _ _ = ret false
    go (x:xs) name1 out ix = do
      name2 <- local (stringInit (paramName x) :: Init ParamString)
      eq    <- istr_eq name1 (constRef name2)
      when eq $ do
        store out ix
        ret true
      go xs name1 out (ix + 1)

-- Rank N type wrapping up the ParamInfoGetter callback
-- to ensure it is always polymorphic on the ref scope
newtype ParamInfoGetter =
  ParamInfoGetter
    { paramInfoGetter :: forall s1 .
        Def ('[ Sint16
              , Ref s1 (Struct "param_value_msg")
              ] :-> IBool)
    }

-- | Create a function to get parameter info by index.
makeGetParamInfo :: [Param ParamReader]
                 -> Task p ParamInfoGetter
makeGetParamInfo params = do
  fr <- fresh
  let p = ParamInfoGetter $ proc ("getParamInfo" ++ (show fr)) $
            \ix ref -> body $ go params ix ref 0
  taskModuleDef $ incl (paramInfoGetter p)
  return p
  where
    count :: Uint16
    count = fromIntegral (length params)
    go [] _ _ _ = ret false
    go (x:xs) ix ref n = do
      when (ix ==? n) $ do
        name <- local (stringInit (paramName x) :: Init ParamString)
        readData (paramData x) (ref ~> PV.param_value)
        store (ref ~> PV.param_count) count
        store (ref ~> PV.param_index) (signCast ix)
        store (ref ~> PV.param_type)  0
        sz_from_istr (ref ~> PV.param_id) (constRef name)
        ret true
      go xs ix ref (n + 1)

-- | Create a function to set a parameter value by index.
makeSetParamValue :: [Param ParamWriter]
                  -> Task p (Def ('[ Sint16
                                   , IFloat
                                   ] :-> IBool))
makeSetParamValue params = do
  fr <- fresh
  let p = proc ("setParamValue" ++ (show fr)) $
            \ix value -> body $ go params ix value 0
  taskModuleDef $ incl p
  return p
  where
    go [] _ _ _ = ret false
    go (x:xs) ix v n = do
      when (ix ==? n) $ do
        val <- local (ival v)
        writeData (paramData x) (constRef val)
        ret true
      go xs ix v (n + 1)

-- | Ivory module for structures defined in this module.
paramModule :: Module
paramModule = package "param_tower" $ do
  defStringType (Proxy :: Proxy ParamString)
  defStruct (Proxy :: Proxy "param_info")
