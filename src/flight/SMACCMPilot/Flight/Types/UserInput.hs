{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Flight.Types.UserInput where

import Ivory.Language

import SMACCMPilot.Mavlink.Messages.RcChannelsOverride

userInputTypeModule :: Module
userInputTypeModule = package "userinput_type" $ do
  defStruct (Proxy :: Proxy "userinput_result")
  defStruct (Proxy :: Proxy "timestamped_rc_override")
  depend rcChannelsOverrideModule

[ivory|
struct userinput_result
  { throttle :: Stored IFloat
  ; roll     :: Stored IFloat
  ; pitch    :: Stored IFloat
  ; yaw      :: Stored IFloat
  ; time     :: Stored Uint32
  }

struct timestamped_rc_override
  { rc_msg  :: Struct "rc_channels_override_msg"
  ; rc_time :: Stored Uint32
  }

|]

