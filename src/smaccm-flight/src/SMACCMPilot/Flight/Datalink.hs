
module SMACCMPilot.Flight.Datalink
  ( datalinkTower
  , plaintextDatalinkTower
  ) where

import Ivory.Tower

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.Datalink.ControllableVehicle
import SMACCMPilot.Flight.Datalink.Commsec

import SMACCMPilot.Commsec.Sizes

datalinkTower :: (e -> FlightPlatform)
              -> CVAPI
              -> (ChanInput CyphertextArray -> ChanOutput CyphertextArray -> Tower e ())
              -> Tower e ()
datalinkTower tofp cvapi dl = do
  rx_ct <- channel
  tx_ct <- channel

  dl (fst rx_ct) (snd tx_ct)

  cv_input <- channel
  cv_output <- controllableVehicle' (snd cv_input) cvapi

  commsecEncodeDatalink todatalink cv_output (fst tx_ct)
  commsecDecodeDatalink todatalink (snd rx_ct) (fst cv_input)

  where
  todatalink = fp_datalink . tofp

plaintextDatalinkTower :: CVAPI
              -> (ChanInput PlaintextArray -> ChanOutput PlaintextArray -> Tower e ())
              -> Tower e ()
plaintextDatalinkTower cvapi dl = do
  cv_input <- channel
  cv_output <- controllableVehicle' (snd cv_input) cvapi

  dl (fst cv_input) cv_output
