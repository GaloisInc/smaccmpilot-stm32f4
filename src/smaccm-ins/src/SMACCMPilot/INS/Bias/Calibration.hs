{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.INS.Bias.Calibration
  ( Calibrate(..)
  , applyCalibrationTower
  , applyCalibrationTower'
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw          as L
import qualified SMACCMPilot.Comm.Ivory.Types.ArmingMode          as A
import           SMACCMPilot.Comm.Ivory.Types.XyzCalibration ()

newtype Calibrate a =
  Calibrate ( forall eff s1 s2 s3
            . (GetAlloc eff ~ Scope s3)
           => ConstRef s1 a
           -> ConstRef s2 (Struct "xyz_calibration")
           -> Ivory eff (ConstRef (Stack s3) a))


applyCalibrationTower :: (IvoryArea a, IvoryZero a)
                      => Calibrate a
                      -> ChanOutput a
                      -> ChanOutput (Struct "xyz_calibration")
                      -> ChanOutput (Struct "control_law")
                      -> Tower e ( ChanOutput a
                                  , ChanOutput (Struct "xyz_calibration"))
applyCalibrationTower calibrate a cal claw = do
  unbiased <- channel
  active_cal <- channel
  applyCalibrationTower' calibrate a cal claw (fst unbiased) (fst active_cal)
  return (snd unbiased, snd active_cal)

applyCalibrationTower' :: (IvoryArea a, IvoryZero a)
                    => Calibrate a
                    -> ChanOutput a
                    -> ChanOutput (Struct "xyz_calibration")
                    -> ChanOutput (Struct "control_law")
                    -> ChanInput  a
                    -> ChanInput  (Struct "xyz_calibration")
                    -> Tower e ()
applyCalibrationTower' calibrate biased_g cal_latest claw unbiased_g cal_active = do
  monitor "applyCalibration" $ do
    law <- state "control_law_"

    pending_cal_ready <- state "pending_cal_ready"
    pending_cal <- state "pending_cal"

    cal <- state "cal"

    handler claw "control_law" $ callback $ \l -> do
      refCopy law l

    handler cal_latest "cal_latest" $ callback $ \c -> do
      refCopy pending_cal c
      store pending_cal_ready true

    handler biased_g "biased_gyro" $ do
      g_emitter <- emitter unbiased_g 1
      c_emitter <- emitter cal_active 1
      callback $ \b -> do
        a <- deref (law ~> L.arming_mode)
        p <- deref pending_cal_ready
        when (a /=? A.armed .&& p) $ do
          refCopy cal pending_cal
          emit c_emitter (constRef cal)
          store pending_cal_ready false
        let (Calibrate c) = calibrate
        unbiased <- c b (constRef cal)
        emit g_emitter unbiased

