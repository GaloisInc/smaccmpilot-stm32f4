{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tower.FooBarAssembly
  ( fooBarAssembly
  ) where

import Ivory.ADL
import Ivory.Language

[ivory|
struct foo_state
  { guided :: Stored Uint8
  ; by     :: Stored Uint8
  ; voices :: Stored Uint8
  }
|]

[ivory|
struct bar_state
  { gameof :: Stored Uint8
  ; pricks :: Stored Uint8
  }
|]

fooSource :: Source (Struct "foo_state")
          -> String -> Object
fooSource fooChan uniquename =
  withSource "fooSource" fooChan $ \fooChanSource ->
  let fooDef = proc ("fooSourceDef" ++ uniquename) $ body $ do
        state <- local (istruct [])
        forever $ do
          v <- deref (state ~> guided)
          store (state ~> guided) (v + 1)
          source fooChanSource (constRef state)
        retVoid
      fooModule = package ("fooSourceMod" ++ uniquename) $ do
        incl fooDef
  in object fooDef fooModule



barSource :: Source (Struct "bar_state")
          -> String -> Object
barSource barChan uniquename =
  withSource "barSource" barChan $ \barChanSource ->
  let barDef = proc ("barSourceDef" ++ uniquename) $ body $ do
        state <- local (istruct [])
        forever $ do
          v <- deref (state ~> pricks)
          store (state ~> pricks) (v + 1)
          source barChanSource (constRef state)
        retVoid
      barModule = package ("barSourceMod" ++ uniquename) $ do
        incl barDef
  in object barDef barModule


fooBarSink :: Sink (Struct "foo_state")
          -> Sink (Struct "bar_state")
          -> String -> Object
fooBarSink fooChan barChan uniquename =
  withSink "fooSink" fooChan $ \fooChanSink ->
  withSink "barSink" barChan $ \barChanSink ->
  let fooBarSinkDef = proc ("fooBarSinkDef" ++ uniquename) $ body $ do
        latestFoo <- local (istruct [])
        latestBar <- local (istruct [])
        forever $ do
          sink fooChanSink latestFoo
          sink barChanSink latestBar
        retVoid
      fooBarSinkMod = package ("fooBarSinkMod" ++ uniquename) $ do
        incl fooBarSinkDef
  in object fooBarSinkDef fooBarSinkMod

fooBarAssembly :: Assembly
fooBarAssembly = adlAssembly $ do
  (source_ss1, sink_ss1) <- connector sharedState
  (source_ss2, sink_ss2) <- connector sharedState
  
  addObject (fooSource source_ss1)
  addObject (barSource source_ss2)
  addObject (fooBarSink sink_ss1 sink_ss2)

