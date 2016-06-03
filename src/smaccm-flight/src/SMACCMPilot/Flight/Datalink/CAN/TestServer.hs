module SMACCMPilot.Flight.Datalink.CAN.TestServer
  ( app
  ) where

import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.Peripheral.CAN.Filter
import Ivory.Language
import Ivory.Tower
import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.Datalink
import SMACCMPilot.Flight.Datalink.ControllableVehicle
import SMACCMPilot.Flight.Datalink.CAN
import SMACCMPilot.Flight.Sensors
import SMACCMPilot.Flight.Light
import SMACCMPilot.Hardware.CAN


app :: (e -> FlightPlatform)
    -> Tower e ()
app tofp = do
  fp <- fmap tofp getEnv

  can <- maybe (fail "fragmentation test requires a CAN peripheral") return $ fp_can fp
  (canRx, canTx, _, _) <- canTower tocc (can_periph can) 125000 (can_RX can) (can_TX can)

  monitor "can_init" $ handler systemInit "can_init" $ do
    callback $ const $ do
      let emptyID = CANFilterID32 (fromRep 0) (fromRep 0) False False
      canFilterInit (can_filters can) [CANFilterBank CANFIFO0 CANFilterMask $ CANFilter32 emptyID emptyID] []

  cvapi@(attrs, _streams) <- controllableVehicleAPI

  plaintextDatalinkTower cvapi (canDatalink canTx canRx)

  mon <- sensorTower tofp attrs
  monitor "uart_dma" mon
  lightTower  tofp attrs

  where
  tocc = fp_clockconfig . tofp
