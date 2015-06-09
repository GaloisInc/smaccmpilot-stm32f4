{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.INS.Filter.RunningAverage
  ( ivoryRunningAverageFilter
  ) where

import GHC.TypeLits (natVal)
import Ivory.Language
import SMACCMPilot.INS.Filter.Filter

ivoryRunningAverageFilter :: forall n
                           . (ANat n)
                          => String -> Proxy n -> (Filter, ModuleDef)
ivoryRunningAverageFilter name proxy = (f, moddef)
  where
  f = Filter
    { filter_init = do
        arrayMap $ \ix -> store (a ! ix) 0
        store offs 0
        store sum_ref 0
    , filter_sample = \input -> do
        o <- deref offs
        store offs ((o ==? (fromIntegral len - 1)) ? (0, o + 1))
        old <- deref (a ! o)
        store (a ! o) input
        s <- deref sum_ref
        store sum_ref (s - old + input)
    , filter_out = do
        s <- deref sum_ref
        return (s / fromIntegral len)
    }

  len = natVal proxy

  named s = name ++ "_avg_" ++ s
  a_area :: MemArea (Array n (Stored IFloat))
  a_area = area (named "store") Nothing
  a = addrOf a_area
  offs_area :: MemArea (Stored (Ix n))
  offs_area = area (named "offs") Nothing
  offs = addrOf offs_area
  sum_area = area (named "sum") Nothing
  sum_ref = addrOf sum_area

  moddef = do
    defMemArea a_area
    defMemArea offs_area
    defMemArea sum_area
