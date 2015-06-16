{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Hardware.PX4IO.Types.Request where

import Ivory.Language

import SMACCMPilot.Hardware.PX4IO.Types.RequestCode

[ivory|
struct px4io_request
  { req_code :: Stored RequestCode
  ; count    :: Stored Uint8
  ; page     :: Stored Uint8
  ; offs     :: Stored Uint8
  ; regs     :: Array 32 (Stored Uint16)
  }
|]

px4ioRequestTypesModule :: Module
px4ioRequestTypesModule = package "px4io_request_type" $ do
  defStruct (Proxy :: Proxy "px4io_request")
