{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module SMACCMPilot.Hardware.Tests.PX4IOPacking (app) where
import Ivory.Artifact
import Ivory.Language
import Ivory.Serialize

import SMACCMPilot.Hardware.PX4IO.Pack
import SMACCMPilot.Hardware.PX4IO.CRC
import SMACCMPilot.Hardware.PX4IO.Types.Buffer
import SMACCMPilot.Hardware.PX4IO.Types.Request
import SMACCMPilot.Hardware.PX4IO.Types.RequestCode

puts :: Def('[IString]:-> Sint32)
puts = importProc "puts" "stdio.h"

app :: ([Module], [Located Artifact])
app = (modules, artifacts)
  where
  modules = [ test_mod
            , px4ioPackModule
            , px4ioCRCModule
            , px4ioBufferTypesModule
            , px4ioRequestTypesModule
            ]

  artifacts = serializeArtifacts

  test_mod = package "test" $ do
    incl main_proc
    incl puts

  main_proc :: Def('[]:->Sint32)
  main_proc = proc "main" $ body $ do
    call_ puts "hello world"
    ret 0

