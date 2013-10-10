{-# LANGUAGE DataKinds #-}

-- | Handle RC Override MAVLink messages from the GCS.

module SMACCMPilot.Flight.UserInput.RCOverride
  ( rcOverrideTask ) where

import Ivory.Language
import Ivory.Tower

--------------------------------------------------------------------------------

rcOverrideTask ::
     DataSink (Struct "rc_channels_override_msg") -- From GCS Rx Task
  -> Task p ()
rcOverrideTask rcOvrRx = undefined
