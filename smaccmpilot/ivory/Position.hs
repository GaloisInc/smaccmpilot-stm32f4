{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Position
  ( positionModule
  ) where

import Ivory.Language

positionModule :: Module
positionModule = package "position" $ do
  defStruct (Proxy :: Proxy "position_result")

-- Because trevor wont let me use haskell comments in a quasiquoter:
 -- degrees *1E7
 -- degrees *1E7
 -- meters * 1000
 -- ground speed m/s*100
 -- ground speed m/s*100
 -- ground speed m/s*100

[ivory|
struct position_result
  { lat     :: Stored Sint32
  ; lon     :: Stored Sint32
  ; gps_alt :: Stored Sint32
  ; vx      :: Stored Sint16
  ; vy      :: Stored Sint16
  ; vz      :: Stored Sint16
  }
|]

