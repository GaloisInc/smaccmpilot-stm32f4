{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Mavlink.Native.Serialize where

import GHC.TypeLits
import Control.Monad
import Data.Sized.Matrix
import Data.Sized.Fin
import Data.Array.IArray
import Data.Serialize

putVector :: (KnownNat n, Serialize a) => Vector n a -> Put
putVector v = forM_ [minBound..maxBound] (\ix -> put (v ! ix))

getVector :: forall n a . (KnownNat n, Serialize a) => Get (Vector n a)
getVector = matrix `fmap` forM [minBound..maxBound] (\(_ :: Fin n) -> get)

instance (KnownNat n, Serialize a) => Serialize (Vector n a) where
  put = putVector
  get = getVector

