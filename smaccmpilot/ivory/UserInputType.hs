{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module UserInputType where

import Ivory.Language

userInputModule :: Module
userInputModule = package "userinput_type" $ do
  defStruct (Proxy :: Proxy "userinput_result")

[ivory|
struct userinput_result
  { armed    :: Stored IBool
  ; throttle :: Stored IFloat 
  ; roll     :: Stored IFloat 
  ; pitch    :: Stored IFloat 
  ; yaw      :: Stored IFloat 
  ; time     :: Stored Uint32
  }
|]

