module Arm32SizeMap where

import Ivory.Opts.CFG (SizeMap(..))

-- Sizes in terms bytes.

sizeMap :: SizeMap
sizeMap = SizeMap
  { stackElemMap = const 1
  , retSize = 4
  }
