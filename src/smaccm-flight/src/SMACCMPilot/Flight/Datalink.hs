
module SMACCMPilot.Flight.Datalink
  ( datalinkTower
  , plaintextDatalinkTower
  , flightDatalinks
  ) where

import Ivory.Language
import Ivory.Tower

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.Datalink.ControllableVehicle
import SMACCMPilot.Flight.Datalink.Commsec
import SMACCMPilot.Flight.Datalink.UART
import SMACCMPilot.Flight.Datalink.CAN

import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Hardware.CAN

import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.Peripheral.CAN.Filter


datalinkTower :: (e -> FlightPlatform)
              -> CVAPI
              -> (ChanInput CyphertextArray -> ChanOutput CyphertextArray -> Tower e (Monitor e ()))
              -> Tower e (Monitor e ())
datalinkTower tofp cvapi dl = do
  rx_ct <- channel
  tx_ct <- channel

  mon   <- dl (fst rx_ct) (snd tx_ct)

  cv_input  <- channel
  cv_output <- controllableVehicle' (snd cv_input) cvapi

  commsecEncodeDatalink todatalink cv_output (fst tx_ct)
  commsecDecodeDatalink todatalink (snd rx_ct) (fst cv_input)
  return mon

  where
  todatalink = fp_datalink . tofp

plaintextDatalinkTower :: CVAPI
              -> (ChanInput PlaintextArray -> ChanOutput PlaintextArray -> Tower e ())
              -> Tower e ()
plaintextDatalinkTower cvapi dl = do
  cv_input <- channel
  cv_output <- controllableVehicle' (snd cv_input) cvapi

  dl (fst cv_input) cv_output


-- Provides a plaintext datalink on fp_can and a secure datalink on fp_telem
flightDatalinks :: (e -> FlightPlatform) -> CVAPI -> Tower e (Monitor e ())
flightDatalinks tofp cvapi = do
  fp  <- fmap tofp getEnv
  mon <- datalinkTower tofp cvapi
    (uartDatalink (fp_clockconfig . tofp) (fp_telem fp) (fp_telem_baud fp))

  can <- maybe (fail "flightDatalinks requires a CAN peripheral") return $ fp_can fp
  (canRx, canTx, _, _) <- canTower tocc (can_periph can) 125000 (can_RX can) (can_TX can)

  monitor "can_init" $ handler systemInit "can_init" $ do
    callback $ const $ do
      let emptyID = CANFilterID32 (fromRep 0) (fromRep 0) False False
      canFilterInit (can_filters can) [CANFilterBank CANFIFO0 CANFilterMask $ CANFilter32 emptyID emptyID] []

  plaintextDatalinkTower cvapi (canDatalink canTx canRx)
  return mon


  where
  tocc = fp_clockconfig . tofp
