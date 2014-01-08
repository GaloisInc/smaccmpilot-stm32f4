{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Flight.Types.UserInput where

import Ivory.Language
import SMACCMPilot.Flight.Types.UISource

userInputTypeModule :: Module
userInputTypeModule = package "userinput_type" $ do
  defStruct (Proxy :: Proxy "userinput_result")

[ivory|
struct userinput_result
  { throttle :: Stored IFloat
  ; roll     :: Stored IFloat
  ; pitch    :: Stored IFloat
  ; yaw      :: Stored IFloat
  ; time     :: Stored Uint32
  ; source   :: Stored UISource
  }

|]

--------------------------------------------------------------------------------
-- PPM type

newtype PPM = PPM Uint16
  deriving ( IvoryType, IvoryOrd, IvoryVar, IvoryExpr
           , IvoryEq, IvoryStore, IvoryInit, Num)

type PPMs = Array 8 (Stored PPM)

instance SafeCast PPM IFloat

instance SafeCast Uint16 PPM where
  safeCast = PPM

instance SafeCast PPM Uint16 where
  safeCast (PPM u) = u

instance Bounded PPM where
  minBound = PPM 800
  maxBound = PPM 2200

ppmHigh, ppmLow, ppmCenter :: PPM
ppmHigh   = 1900
ppmLow    = 1100
ppmCenter = 1500

--------------------------------------------------------------------------------

