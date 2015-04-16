{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.CANMessages where

import Ivory.Language
import SMACCM.Fragment
import SMACCMPilot.Hardware.GPS.Types ()
import SMACCMPilot.Hardware.Types.Accelerometer ()
import SMACCMPilot.Hardware.Types.Barometer ()
import SMACCMPilot.Hardware.Types.Gyroscope ()
import SMACCMPilot.Hardware.Types.Magnetometer ()

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
