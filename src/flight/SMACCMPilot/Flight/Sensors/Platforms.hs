
module SMACCMPilot.Flight.Sensors.Platforms where

import Ivory.Language
import SMACCMPilot.Flight.Platforms

class SensorOrientation p where
  -- Describe whether board is inverted (component side up)
  sensorOrientation :: Proxy p -> IBool

instance SensorOrientation PX4FMU17_IOAR where
  sensorOrientation _ = false

instance SensorOrientation PX4FMU17_Bare where
  sensorOrientation _ = true

