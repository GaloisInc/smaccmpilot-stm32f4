
module SMACCMPilot.Driver.Gpio where

import Smaccm.Stm32f4.GPIO

import Ivory.Language

gpioModule :: Module
gpioModule = package "gpio_module" gpioDefs

