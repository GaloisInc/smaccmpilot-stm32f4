{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.CANMessages where

import Ivory.Language
import Ivory.Tower.HAL.Bus.CAN.Fragment
import Ivory.Tower.HAL.Sensor.Accelerometer ()
import Ivory.Tower.HAL.Sensor.Barometer ()
import Ivory.Tower.HAL.Sensor.Gyroscope ()
import Ivory.Tower.HAL.Sensor.Magnetometer ()
import SMACCMPilot.Hardware.GPS.Types ()

gyroType :: MessageType (Struct "gyroscope_sample")
gyroType = messageType 0x001 False (Proxy :: Proxy 26) -- 200Hz, 5 fragments

accelType :: MessageType (Struct "accelerometer_sample")
accelType = messageType 0x011 False (Proxy :: Proxy 26) -- 200Hz, 5 fragments

magType :: MessageType (Struct "magnetometer_sample")
magType = messageType 0x021 False (Proxy :: Proxy 22) -- 50Hz, 3 fragments

baroType :: MessageType (Struct "barometer_sample")
baroType = messageType 0x031 False (Proxy :: Proxy 18) -- 50Hz, 3 fragments

gpsType :: MessageType (Struct "position")
gpsType = messageType 0x041 False (Proxy :: Proxy 46) -- 1Hz?, 6 fragments
