
module Ivory.BSP.STM32.SearchDir where

import System.FilePath

import qualified Paths_ivory_bsp_stm32

searchDir :: IO FilePath
searchDir = do
  base <- Paths_ivory_bsp_stm32.getDataDir
  return $ base </> "support"

