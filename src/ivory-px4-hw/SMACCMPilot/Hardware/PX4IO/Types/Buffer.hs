{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Hardware.PX4IO.Types.Buffer where

import Ivory.Language

[ivory|
string struct PX4IOBuffer 68
|]

px4ioBufferTypesModule :: Module
px4ioBufferTypesModule = package "px4io_buffer_type" $ do
  defStringType (Proxy :: Proxy PX4IOBuffer)
