{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Motors.Decoder
  ( fmuPwmMotorDecoder
  ) where

import Ivory.Language

import qualified SMACCMPilot.Comm.Ivory.Types.QuadcopterMotors as M

fmuPwmMotorDecoder :: ConstRef s (Struct "quadcopter_motors")
                   -> Ivory (AllocEffects cs)
                         (ConstRef (Stack cs) (Array 4 (Stored IFloat)))
fmuPwmMotorDecoder ms = do
  fl <- deref (ms ~> M.frontleft) -- m3
  fr <- deref (ms ~> M.frontright) -- m2
  br <- deref (ms ~> M.backright) -- m4
  bl <- deref (ms ~> M.backleft) -- m2
  -- Canonical ArduCopter motor ordering, see:
  -- http://copter.ardupilot.com/wiki/connecting-your-rc-input-and-motors/
  l <- local (iarray [ival fr, ival bl, ival fl, ival br])
  return (constRef l)


