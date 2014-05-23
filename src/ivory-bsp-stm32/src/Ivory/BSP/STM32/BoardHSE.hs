
module Ivory.BSP.STM32.BoardHSE where

import Ivory.Language

class BoardHSE p where
  hseFreqHz :: Proxy p -> Integer

hseFreq :: (BoardHSE p) => Proxy p -> Uint32
hseFreq p = fromIntegral (hseFreqHz p)

