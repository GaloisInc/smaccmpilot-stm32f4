{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
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

----------------------------------------------------------------------
-- Parameter Lookup

-- | Run-time information about a parameter.
[ivory|
 struct param_info
 { param_name      :: Struct "ivory_string"   -- XXX IStr
 ; param_index     :: Stored Sint32
 ; param_value     :: Stored IFloat
 }
|]

-- | Create a function to look up a parameter index by name.
makeGetParamIndex :: [Param f]
                  -> Def ('[ ConstRef s1 IStr
                           , Ref      s2 (Stored Sint16)
                           ] :-> IBool)
makeGetParamIndex params = proc "getParamIndex" $ \name ref -> body $ go params name ref 0
  where
    go [] _ _ _ = ret false
    go (x:xs) name1 out ix = do
      name2 <- local (istr_lit (paramName x))
      eq    <- call istr_eq name1 (constRef name2)
      when eq $ do
        store out ix
        ret true
      go xs name1 out (ix + 1)

-- | Create a function to get parameter info by index.
makeGetParamInfo :: [Param ParamReader]
                 -> Def ('[ Sint16
                          , Ref s1 (Struct "param_value_msg")
                          ] :-> IBool)
makeGetParamInfo params = proc "getParamInfo" $ \ix ref -> body $ go params ix ref 0
  where
    count :: Uint16
    count = fromIntegral (length params)
    go [] _ _ _ = ret false
    go (x:xs) ix ref n = do
      when (ix ==? n) $ do
        name <- local (istr_lit (paramName x))
        readData (paramData x) (ref ~> PV.param_value)
        store (ref ~> PV.param_count) count
        store (ref ~> PV.param_index) (signCast ix)
        store (ref ~> PV.param_type)  0
        sz_from_istr (ref ~> PV.param_id) (constRef name)
        ret true
      go xs ix ref (n + 1)

-- | Create a function to set a parameter value by index.
makeSetParamValue :: [Param ParamWriter]
                  -> Def ('[ Sint16
                           , IFloat
                           ] :-> IBool)
makeSetParamValue params = proc "setParamValue" $ \ix value -> body $ go params ix value 0
  where
    go [] _ _ _ = ret false
    go (x:xs) ix v n = do
      when (ix ==? n) $ do
        val <- local (ival v)
        writeData (paramData x) (constRef val)
        ret true
      go xs ix v (n + 1)

