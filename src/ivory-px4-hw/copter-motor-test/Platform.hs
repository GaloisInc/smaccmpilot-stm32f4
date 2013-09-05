{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module Platform where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Frontend

import qualified SMACCMPilot.Hardware.PX4IOAR as IOAR
import qualified SMACCMPilot.Hardware.PX4FMU17 as Bare

class RawMotorControl p where
  rawMotorControl :: (SingI n) => ChannelSink n (Array 4 (Stored IFloat)) -> Tower p ()

data PX4FMU17_IOAR = PX4FMU17_IOAR
data PX4FMU17_Bare = PX4FMU17_Bare

instance RawMotorControl PX4FMU17_IOAR where
  rawMotorControl = IOAR.motorControlTower cpystack

instance RawMotorControl PX4FMU17_Bare where
  rawMotorControl = Bare.motorControlTower cpystack

cpystack :: ConstRef s (Array 4 (Stored IFloat))
         -> Ivory (AllocEffects cs)
              (ConstRef (Stack cs) (Array 4 (Stored IFloat)))
cpystack v = do
  l <- local (iarray [])
  arrayMap $ \i -> deref (v ! i) >>= store (l ! i)
  return (constRef l)

motorPlatforms :: (forall p . (RawMotorControl p) => Tower p ()) -> [(String, Twr)]
motorPlatforms app =
    [("px4fmu17_ioar", Twr (app :: Tower PX4FMU17_IOAR ()))
    ,("px4fmu17_bare", Twr (app :: Tower PX4FMU17_Bare ()))
    ]
