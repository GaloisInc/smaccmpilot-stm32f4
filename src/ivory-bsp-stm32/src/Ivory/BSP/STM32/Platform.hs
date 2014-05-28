{-# LANGUAGE TypeFamilies #-}

module Ivory.BSP.STM32.Platform
  ( Platform(..)
  ) where

class Platform p where
  data Interrupt p
  data GPIOPin p

