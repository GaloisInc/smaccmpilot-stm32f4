{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Takes input from PPM RC controller and from the RC Overide task and
-- multiplexes them for output to the motor control.

module SMACCMPilot.Flight.UserInput.Mux
  ( userInputMuxTask
  ) where

import Ivory.Language
import Ivory.Tower

--------------------------------------------------------------------------------

userInputMuxTask :: -- From PPM task
                    DataSink (Struct "userinput_result")
                    -- From RCOverride task
                 -> DataSink (Struct "userinput_result")
                    -- To motor control task
                 -> DataSource (Struct "userinput_result")
                 -> Task p ()
userInputMuxTask ppmRx rcOvrRx res = undefined

--------------------------------------------------------------------------------

