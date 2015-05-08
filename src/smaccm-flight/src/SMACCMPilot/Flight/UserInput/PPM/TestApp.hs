

module SMACCMPilot.Flight.UserInput.PPM.TestApp
  ( app
  ) where

import Ivory.Language
import Ivory.Tower

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.Datalink
import SMACCMPilot.Flight.UserInput.PPM

import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw      as CL ()
import qualified SMACCMPilot.Comm.Ivory.Types.ControlSource   as CS
import qualified SMACCMPilot.Comm.Ivory.Types.TimeMicros      as T
import qualified SMACCMPilot.Comm.Ivory.Types.UserInput       as UI ()
import qualified SMACCMPilot.Comm.Ivory.Types.UserInputResult as UIR

import SMACCMPilot.Comm.Tower.Attr
import SMACCMPilot.Comm.Tower.Interface.ControllableVehicle

app :: (e -> FlightPlatform)
    -> Tower e ()
app tofp = do

  (attrs, _streams) <- datalinkTower tofp

  (ui, cl) <- ppmInputTower toppm tocc

  monitor "forward_ppmInputTower" $ do
    handler ui "ui" $ do
      e <- attrEmitter (userInput attrs)
      callback $ \v -> do
        now <- fmap (T.TimeMicros . toIMicroseconds) getTime
        o <- local (istruct [ UIR.time .= ival now
                            , UIR.source .= ival CS.ppm ])
        refCopy (o ~> UIR.ui) v
        emit e (constRef o)
    handler cl "cl" $ do
      e <- attrEmitter (controlLaw attrs)
      callback $ \v -> do
        emit e v
  where
  tocc = fp_clockconfig . tofp
  toppm = fp_ppm . tofp
