
module Ivory.BSP.STM32F4.SearchDir where

import System.FilePath

import qualified Paths_ivory_bsp_stm32f4

searchDir :: IO FilePath
searchDir = do
  base <- Paths_ivory_bsp_stm32f4.getDataDir
  return $ base </> "support"

