{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Takes input from PPM RC controller and from the RC Overide task and
-- multiplexes them for output to the motor control.

module SMACCMPilot.Flight.UserInput.Mux
  ( userInputMuxTask
  , armedMuxTask
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import qualified SMACCMPilot.Flight.Types.UserInput  as T
import qualified SMACCMPilot.Flight.Types.FlightMode as FM

--------------------------------------------------------------------------------

-- Timeout to revert back to the RC PPM controller.
mavTimeout :: Uint32
mavTimeout = 500

--------------------------------------------------------------------------------

armedMuxTask :: DataSink (Array 8 (Stored Uint16)) -- PPM signals
             -> DataSource (Stored IBool)          -- Mux'ed arming output
             -> DataSink (Stored IBool)            -- MAVLink arming input
             -> Task p ()
armedMuxTask = undefined

--------------------------------------------------------------------------------

userInputMuxTask :: -- From PPM task
                    DataSink (Struct "userinput_result")
                    -- From RCOverride task
                 -> DataSink (Struct "userinput_result")
                    -- To motor control task
                 -> DataSource (Struct "userinput_result")
                 -> Task p ()
userInputMuxTask snk_rc_ppm snk_mav_ppm src_res = do
  ppmReader <- withDataReader snk_rc_ppm  "snk_rc_ppm"
  mavReader <- withDataReader snk_mav_ppm "snk_mav_ppm"
  resWriter <- withDataWriter src_res     "src_res"

  rcLocal   <- taskLocal "rcLocal"
  mavLocal  <- taskLocal "mavLocal"

  let writeOutput :: (GetAlloc eff ~ Scope s0)
                  => Ref s1 (Struct "userinput_result")
                  -> Ivory eff ()
      writeOutput = writeData resWriter . constRef

  onPeriod 50 $ \now -> do
    readData ppmReader rcLocal
    readData mavReader mavLocal

    lastMavTime <- deref (mavLocal ~> T.time)
    -- Time is monotomic.
    assert (now >=? lastMavTime)

    -- XXX
    -- isArmed <- deref (mavLocal ~> FM.armed)

    cond_
      [   -- Not armed: don't listen to MAVLink messages.
          true -- iNot isArmed XXX
      ==> writeOutput rcLocal
          -- No MAVLink message has arrived in the past mavTime milliseconds.
          -- Ignore MAV input.
      ,   (now >=? (lastMavTime + mavTimeout))
      ==> writeOutput rcLocal
          -- Otherwise, we can use the MAVLink RC override.
      ,   true
      ==> writeOutput mavLocal
      ]

  taskModuleDef $
    depend T.userInputTypeModule

--------------------------------------------------------------------------------

