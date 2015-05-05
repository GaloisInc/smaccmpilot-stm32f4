{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Motors.Platforms
  ( MotorOutput
  , motorOutput
  ) where

import Ivory.Language
import Ivory.Tower

import SMACCMPilot.Flight.Platforms

import qualified SMACCMPilot.Flight.Types.Motors as M
import qualified SMACCMPilot.Hardware.PX4IOAR as PX4IOAR
import qualified SMACCMPilot.Hardware.PX4FMU17 as PX4FMU17

class MotorOutput p where
  motorOutput :: ChannelSink (Struct "motors") -> Tower p ()

instance MotorOutput PX4FMU17_IOAR where
  motorOutput = PX4IOAR.motorControlTower ioarMotorDecoder

instance MotorOutput PX4FMU17_Bare where
  motorOutput = PX4FMU17.motorControlTower fmuPwmMotorDecoder

ioarMotorDecoder :: ConstRef s (Struct "motors")
                 -> Ivory (AllocEffects cs)
                       (ConstRef (Stack cs) (Array 4 (Stored IFloat)))
ioarMotorDecoder ms = do
  m1 <- deref (ms ~> M.frontleft)
  m2 <- deref (ms ~> M.frontright)
  m3 <- deref (ms ~> M.backright)
  m4 <- deref (ms ~> M.backleft)
  l <- local (iarray [ival m1, ival m2, ival m3, ival m4])
  return (constRef l)

fmuPwmMotorDecoder :: ConstRef s (Struct "motors")
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


