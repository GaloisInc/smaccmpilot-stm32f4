{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Platform where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Frontend

import Ivory.BSP.STM32F4.RCC
import Ivory.BSP.STM32F4.Signalable

import qualified SMACCMPilot.Hardware.PX4IOAR as IOAR
import qualified SMACCMPilot.Hardware.PX4FMU17 as Bare

f24MHz :: Uint32
f24MHz = 24000000

class RawMotorControl p where
  rawMotorControl :: ChannelSink (Array 4 (Stored IFloat)) -> Tower p ()

data PX4FMU17_IOAR = PX4FMU17_IOAR
data PX4FMU17_Bare = PX4FMU17_Bare

stm32f4SignalableInstance ''PX4FMU17_IOAR
stm32f4SignalableInstance ''PX4FMU17_Bare

instance RawMotorControl PX4FMU17_IOAR where
  rawMotorControl = IOAR.motorControlTower cpystack

instance BoardHSE PX4FMU17_IOAR where
  hseFreq _ = f24MHz

instance RawMotorControl PX4FMU17_Bare where
  rawMotorControl = Bare.motorControlTower cpystack

instance BoardHSE PX4FMU17_Bare where
  hseFreq _ = f24MHz

cpystack :: ConstRef s (Array 4 (Stored IFloat))
         -> Ivory (AllocEffects cs)
              (ConstRef (Stack cs) (Array 4 (Stored IFloat)))
cpystack v = do
  l <- local (iarray [])
  arrayMap $ \i -> deref (v ! i) >>= store (l ! i)
  return (constRef l)

motorPlatforms :: (forall p . (RawMotorControl p, BoardHSE p, STM32F4Signal p) 
                    => Tower p ())
               -> [(String, Twr)]
motorPlatforms app =
    [("px4fmu17_ioar", Twr (app :: Tower PX4FMU17_IOAR ()))
    ,("px4fmu17_bare", Twr (app :: Tower PX4FMU17_Bare ()))
    ]
