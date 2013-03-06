{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FooBarTasks
  ( fooSource, barSource, fooBarSink, fooBarTypesModule
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Connections.FreeRTOS
import qualified Ivory.OS.FreeRTOS as OS

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
          -> String -> Task
fooSource fooChan uniquename =
  withSource "fooSource" fooChan $ \fooChanSource ->
  let fooDef = proc ("fooSourceDef" ++ uniquename) $ body $ do
        initTime <- call OS.getTimeMillis
        lastTime <- local (ival initTime)
        state <- local (istruct [])
        forever $ do
          v <- deref (state ~> guided)
          store (state ~> guided) (v + 1)
          source fooChanSource (constRef state)
          let period = 50
          call_ OS.delayUntil lastTime period
        retVoid
      fooModuleName = "fooSourceMod" ++ uniquename
      fooModuleDefs = do
        depend OS.taskModule
        depend fooBarTypesModule
        incl fooDef
  in task fooDef fooModuleName fooModuleDefs



barSource :: Source (Struct "bar_state")
          -> String -> Task
barSource barChan uniquename =
  withSource "barSource" barChan $ \barChanSource ->
  let barDef = proc ("barSourceDef" ++ uniquename) $ body $ do
        initTime <- call OS.getTimeMillis
        lastTime <- local (ival initTime)
        state <- local (istruct [])
        forever $ do
          v <- deref (state ~> pricks)
          store (state ~> pricks) (v + 1)
          source barChanSource (constRef state)
          let period = 100
          call_ OS.delayUntil lastTime period
        retVoid
      barModuleName = "barSourceMod" ++ uniquename
      barModuleDefs = do
        depend OS.taskModule
        depend fooBarTypesModule
        incl barDef
  in task barDef barModuleName barModuleDefs


fooBarSink :: Sink (Struct "foo_state")
          -> Sink (Struct "bar_state")
          -> String -> Task
fooBarSink fooChan barChan uniquename =
  withSink "fooSink" fooChan $ \fooChanSink ->
  withSink "barSink" barChan $ \barChanSink ->
  let fooBarSinkDef = proc ("fooBarSinkDef" ++ uniquename) $ body $ do
        initTime <- call OS.getTimeMillis
        lastTime <- local (ival initTime)
        latestFoo <- local (istruct [])
        latestBar <- local (istruct [])
        forever $ do
          sink fooChanSink latestFoo
          sink barChanSink latestBar
          let period = 50
          call_ OS.delayUntil lastTime period
        retVoid
      fooBarSinkModName = "fooBarSinkMod" ++ uniquename
      fooBarSinkModDefs = do
        depend OS.taskModule
        depend fooBarTypesModule
        incl fooBarSinkDef
  in task fooBarSinkDef fooBarSinkModName fooBarSinkModDefs

fooBarTypesModule :: Module
fooBarTypesModule = package "fooBarTypes" $ do
  defStruct (Proxy :: Proxy "bar_state")
  defStruct (Proxy :: Proxy "foo_state")

