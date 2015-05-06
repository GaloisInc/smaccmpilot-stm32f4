{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Motors
  ( motorMixer
  ) where

import Ivory.Language
import Ivory.Tower

import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw       as CL
import qualified SMACCMPilot.Comm.Ivory.Types.ArmingMode       as A
import qualified SMACCMPilot.Comm.Ivory.Types.QuadcopterMotors as M
import           SMACCMPilot.Comm.Tower.Attr

import SMACCMPilot.Flight.Motors.Mixing

motorMixer :: (AttrReadable a1, AttrReadable a2, AttrWritable a3)
           => a1 (Struct "control_output")
           -> a2 (Struct "control_law")
           -> a3 (Struct "quadcopter_motors")
           -> Tower e ()
motorMixer ctl_out_attr cl_attr motors_attr =
  monitor "motorMixer" $ do
    cl <- attrState cl_attr
    handler systemInit "init_motors" $ do
      e <- attrEmitter motors_attr
      callback $ const $ do
        d <- disabled
        emit e d
    attrHandler ctl_out_attr $ do
      e <- attrEmitter motors_attr
      callback $ \ctl -> do
        armed <- deref (cl ~> CL.arming_mode)
        let output = emit e
        ifte_ (armed ==? A.armed)
          (mixer ctl >>= output)
          (disabled >>= output)

disabled :: (GetAlloc eff ~ Scope cs)
         => Ivory eff (ConstRef (Stack cs) (Struct "quadcopter_motors"))
disabled = do
  v <- local $ istruct zeroes
  return (constRef v)
  where
  zeroes = [ M.frontleft  .= ival 0
           , M.frontright .= ival 0
           , M.backleft   .= ival 0
           , M.backright  .= ival 0
           ]

