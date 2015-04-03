
module SMACCMPilot.Comm.Ivory.Server.TestApp
  ( app
  ) where

import Ivory.Language
import Ivory.Tower
import SMACCMPilot.Hardware.Tests.Platforms

import SMACCMPilot.Comm.Ivory.Server

app :: (e -> PX4Platform) -> Tower e ()
app _topx4 = do
  commTower
