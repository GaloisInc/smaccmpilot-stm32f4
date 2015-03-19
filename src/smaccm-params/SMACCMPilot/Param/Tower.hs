{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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

-- | Wrapper type for a data source of parameter values.
data ParamSource =
  ParamSource
    { psrc_chan :: ChanInput (Stored IFloat)
    , psrc_name :: String
    }

-- | Wrapper type for a data sink of parameter values.
data ParamSink =
  ParamSink
    { psnk_chan :: ChanOutput (Stored IFloat)
    , psnk_name :: String
    , psnk_ival :: Float
    }

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
    go n v = do
      (psrc, psink) <- channel
      return (PortPair (ParamSource psrc n) (ParamSink psink n v))

-- | Shorthand type for a reader for parameter values.
data ParamState = forall s . ParamState (Ref s (Stored IFloat))
-- | Open data readers for a parameter tree containing sinks.
paramState :: (Traversable a)
            => a ParamSink
            -> Monitor p (a ParamState)
paramState = traverse mkreader
  where
  mkreader psnk = do
    s <- stateInit ("param_reader_" ++ psnk_name psnk)
                   (ival (ifloat (psnk_ival psnk)))
    handler (psnk_chan psnk) ("param_update_" ++ psnk_name psnk) $ do
      callbackV $ \v -> store s v
    return (ParamState s)

-- | Read the float values of a parameter tree.
paramRead :: (GetAlloc eff ~ Scope s, Traversable a)
          => a ParamState
          -> Ivory eff (a IFloat)
paramRead = traverse (\(ParamState s) -> deref s)

-- | Shorthand for reading a single unwrapped Param value
paramGetState :: (GetAlloc eff ~ Scope s)
              => Param ParamState -> Ivory eff IFloat
paramGetState r = do
  p <- paramRead r
  return (paramData p)

-- | Shorthand type for a writer for parameter values.
type ParamWriter = Emitter (Stored IFloat)

-- | Open data writers for a parameter tree containing sources.
paramWriter :: (Traversable a)
            => a ParamSource
            -> Handler e b (a ParamWriter)
paramWriter = traverse mkwriter
  where
  mkwriter psrc = emitter (psrc_chan psrc) 1


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
                  -> Monitor p ParamIndexGetter
makeGetParamIndex params = do
  fr <- fresh
  let p = ParamIndexGetter $ proc ("getParamIndex" ++ (show fr)) $
            \name ref -> body $ go params name ref 0
  monitorModuleDef $ incl (paramIndexGetter p)
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
makeGetParamInfo :: [Param ParamState]
                 -> Monitor p ParamInfoGetter
makeGetParamInfo params = do
  fr <- fresh
  let p = ParamInfoGetter $ proc ("getParamInfo" ++ (show fr)) $
            \ix ref -> body $ go params ix ref 0
  monitorModuleDef $ incl (paramInfoGetter p)
  return p
  where
    count :: Uint16
    count = fromIntegral (length params)
    go [] _ _ _ = ret false
    go (x:xs) ix ref n = do
      when (ix ==? n) $ do
        name <- local (stringInit (paramName x) :: Init ParamString)
        s <- paramGetState x
        store (ref ~> PV.param_value) s
        store (ref ~> PV.param_count) count
        store (ref ~> PV.param_index) (signCast ix)
        store (ref ~> PV.param_type)  0
        sz_from_istr (ref ~> PV.param_id) (constRef name)
        ret true
      go xs ix ref (n + 1)


-- | Create a function to set a parameter value by index.
makeSetParamValue :: (GetAlloc eff ~ Scope s)
                  => [Param ParamSource]
                  -> Handler e b (Sint16 -> IFloat -> Ivory eff IBool)
makeSetParamValue param_src = do
  pws <- mapM paramWriter param_src
  return $ \ix value -> do
    rv <- local (ival false)
    go pws ix value 0 rv
  where
    go [] _ _ _ rv = deref rv
    go (x:xs) ix v n rv = do
      when (ix ==? n) $ do
        val <- local (ival v)
        emit (paramData x) (constRef val)
        store rv true
      go xs ix v (n + 1) rv

-- | Ivory module for structures defined in this module.
paramModule :: Module
paramModule = package "param_tower" $ do
  defStringType (Proxy :: Proxy ParamString)
  defStruct (Proxy :: Proxy "param_info")
