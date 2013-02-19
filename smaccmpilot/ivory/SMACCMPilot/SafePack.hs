{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
--
-- SafePack.hs --- Checked binary packing/unpacking.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module SMACCMPilot.SafePack (
  -- * Classes
  MonadIvory, liftI,

  -- * Types
  PackM, UnpackM,

  -- * Packing
  mpack, marrayPack, packInto, packInto_,

  -- * Unpacking
  munpack, marrayUnpack, unpackFrom, unpackFrom_
) where

import GHC.TypeLits
import Ivory.Language
import MonadLib hiding (local)

import Smaccm.Mavlink.Pack

-- TODO: We probably need a "skip" function that adjusts the offset in
-- PackM/UnpackM in case we want to skip fields.

-- | Type class for lifting Ivory actions into pack/unpack monads.
--
-- This should live elsewhere if we use more monad transformers on top
-- of Ivory.
class MonadIvory m where
  -- | Lift an Ivory action into the monad "m".
  liftI :: Ivory s r a -> m s r a

----------------------------------------------------------------------
-- Packing Monad

-- | Monad for safely packing values into an array with bounds
-- checking (at code-generation time).
newtype PackM s r a =
  PackM {
    runPackM :: forall s1 len. (SingI len)
             => (StateT Int
                 (ReaderT (Ref s1 (Array len (Stored Uint8)))
                  (Ivory s r)) a)
  }

instance Monad (PackM s r) where
  return x = PackM (return x)
  (PackM m) >>= f = PackM (m >>= runPackM . f)

instance MonadIvory PackM where
  liftI m = PackM (lift (lift m))

-- | Dereference a "ConstRef" and pack the value into the array stored
-- in the context established by "packInto".  An error will be thrown
-- at code generation time if too much data is packed into the array.
--
-- XXX maybe this should just take an "a" instead of a reference?  It
-- is nice for symmetry with "munpack" though.
mpack :: (MavlinkPackable a) => ConstRef s1 (Stored a) -> PackM s r ()
mpack ref = PackM $ do
  buf    <- ask
  offset <- get
  val    <- lift $ lift $ deref ref
  let new_offset = offset + packedSize val
  if new_offset > arrayLen buf
    then error $ "packing " ++ (show new_offset) ++ " bytes into "
              ++ "array of length " ++ (show (arrayLen buf :: Int))
    else return ()
  lift $ lift $ call_ pack (toCArray buf) (fromIntegral offset) val
  set new_offset

-- | Pack an array of packable values into the array stored in the
-- context established by "packInto".  An error will be thrown at code
-- generation time if too much data is packed into the array.
--
-- XXX array ref should be const
marrayPack :: forall a len s s1 r.
              (MavlinkPackable a, SingI len, IvoryType r)
           => Ref s1 (Array len (Stored a))
           -> PackM s r ()
marrayPack arr = PackM $ do
  buf    <- ask
  offset <- get
  let new_offset = offset + (arrayLen arr * packedSize (undefined :: a))
  if new_offset > arrayLen buf
    then error $ "packing " ++ (show new_offset) ++ " bytes into "
              ++ "array of length " ++ (show (arrayLen buf :: Int))
    else return ()
  lift $ lift $ arrayPack (toCArray buf) (fromIntegral offset) arr
  set new_offset

-- | Begin a context to pack values into a byte array, given an
-- initial offset.  Returns the final offset.
packInto :: (SingI len)
         => (Ref s1 (Array len (Stored Uint8))) -- buf
         -> Int                                 -- offset
         -> PackM s2 r ()                       -- body
         -> Ivory s2 r Int
packInto buf offset m =
  runReaderT buf (liftM snd (runStateT offset (runPackM m)))

-- | Like "packInto" but doesn't return the final offset.
packInto_ :: (SingI len)
          => (Ref s1 (Array len (Stored Uint8))) -- buf
          -> Int                                 -- offset
          -> PackM s2 r ()                       -- body
          -> Ivory s2 r ()
packInto_ buf offset m = packInto buf offset m >> return ()

----------------------------------------------------------------------
-- Unpacking Monad

-- | Monad for safely unpacking values from an array with bounds
-- checking (at code-generation time).
newtype UnpackM s r a =
  UnpackM {
    runUnpackM :: forall s1 len. (SingI len)
               => (StateT Int
                   (ReaderT (ConstRef s1 (Array len (Stored Uint8)))
                    (Ivory s r)) a)
  }

instance Monad (UnpackM s r) where
  return x = UnpackM (return x)
  (UnpackM m) >>= f = UnpackM (m >>= runUnpackM . f)

instance MonadIvory UnpackM where
  liftI m = UnpackM (lift (lift m))

-- | Unpack a value from the array stored in the context established
-- by "unpackFrom" into a reference.  An error will be thrown at code
-- generation time if too much data is unpacked from the array.
munpack :: forall a s s1 r.
           (MavlinkPackable a, IvoryStore s1 a)
        => Ref s1 (Stored a) -> UnpackM s r ()
munpack ref = UnpackM $ do
  buf    <- ask
  offset <- get
  let new_offset = offset + packedSize (undefined :: a)
  if new_offset > arrayLen buf
    then error $ "unpacking " ++ (show new_offset) ++ " bytes from "
              ++ "array of length " ++ (show (arrayLen buf :: Int))
    else return ()
  lift $ lift $ do
    val <- call unpack (toCArray buf) (fromIntegral offset)
    store ref val
  set new_offset

-- | Unpack an array of values from the array stored in the context
-- established by "unpackFrom".  An error will be thrown at code
-- generation time if too much data is unpacked from the array.
marrayUnpack :: forall a len s s1 r.
                (MavlinkPackable a, SingI len, IvoryType r, IvoryStore s1 a)
             => Ref s1 (Array len (Stored a))
             -> UnpackM s r ()
marrayUnpack arr = UnpackM $ do
  buf    <- ask
  offset <- get
  let new_offset = offset + (arrayLen arr * packedSize (undefined :: a))
  if new_offset > arrayLen buf
    then error $ "unpacking " ++ (show new_offset) ++ " bytes from "
              ++ "array of length " ++ (show (arrayLen buf :: Int))
    else return ()
  lift $ lift $ arrayUnpack (toCArray buf) (fromIntegral offset) arr
  set new_offset

-- | Begin a context to unpack values from a byte array, given an
-- initial offset.  Returns the final offset.
unpackFrom :: (SingI len)
           => (ConstRef s1 (Array len (Stored Uint8))) -- buf
           -> Int                                      -- offset
           -> UnpackM s2 r ()                          -- body
           -> Ivory s2 r Int
unpackFrom buf offset m =
  runReaderT buf (liftM snd (runStateT offset (runUnpackM m)))

-- | Like "unpackFrom" but doesn't return the final offset.
unpackFrom_ :: (SingI len)
            => (ConstRef s1 (Array len (Stored Uint8))) -- buf
            -> Int                                      -- offset
            -> UnpackM s2 r ()                          -- body
            -> Ivory s2 r ()
unpackFrom_ buf offset m = unpackFrom buf offset m >> return ()
