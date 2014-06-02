{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.BSP.STM32F405.Init
  ( stm32f405InitTower
  ) where

import Ivory.Language
import Ivory.Tower

import Ivory.BSP.STM32.PlatformClock
import Ivory.BSP.STM32.Init (stm32InitModule)
import Ivory.BSP.STM32F405.VectorTable (vector_table)

stm32f405InitTower :: forall p . (PlatformClock p) => Tower p ()
stm32f405InitTower = do
  towerArtifact vectorArtifact
  towerModule (stm32InitModule (Proxy :: Proxy p))
  where
  vectorArtifact = Artifact
    { artifact_filepath = "stm32f405_vectors.s"
    , artifact_contents = vector_table
    , artifact_tag      = "SOURCES"
    }
