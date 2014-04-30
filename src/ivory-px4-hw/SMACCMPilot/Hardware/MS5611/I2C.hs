{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Hardware.MS5611.I2C where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.StateMachine
import Ivory.BSP.STM32F4.I2C

import SMACCMPilot.Hardware.MS5611.Regs

commandRequest :: (GetAlloc eff ~ Scope s)
           => I2CDeviceAddr
           -> Command
           -> Ivory eff (ConstRef (Stack s) (Struct "i2c_transaction_request"))
commandRequest = undefined

