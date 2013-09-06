{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--
-- Param/Base.hs --- Generic hierarchical parameter library.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module SMACCMPilot.Param.Base where

import Control.Applicative
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import MonadLib.Derive
import MonadLib.Monads as M

----------------------------------------------------------------------
-- Parameter Library
--
-- To-do List:
--
-- * Add a 'paramIndex' field that is used by MAVlink when
--   addressing parameters by index?
--
-- * Build by name and by index parameter tables.
--
-- * Wire this up to the GCS tasks for MAVlink.
--
-- * Wire this up to parameter storage in EEPROM.

-- | A MAVlink parameter.
--
-- 'f' is the data type of the underlying storage for the parameter.
-- When used with tower, 'f' will be a data source/sink or
-- reader/writer.
data Param f = Param
  { paramName     :: String
  , paramDefault  :: Float
  -- , paramIndex    :: Int
  , paramData     :: f
  } deriving (Functor, Foldable, Traversable)

-- | Constructor for a parameter value in the monad 'm' from name
-- and default value.
type Constr f m = String -> Float -> m f

-- | Reader state in the 'ParamT' monad.  The 'prefix' is locally
-- bound inside a parameter group, and 'constr' is bound in
-- 'paramInit' to the function used by 'param' to construct parameter
-- values.
data PT_R f m = PT_R
  { pt_r_prefix :: String
  , pt_r_constr :: Constr f m
  }

-- | Parameter initialization type.  Users will generally use the
-- 'Applicative' instance to combine calls to 'param' and 'group'
-- with record data constructors.
data ParamT f m a = PT { unPT :: ReaderT (PT_R f m) (WriterT [Param f] m) a }

-- | Run a parameter's constructor function given a function to
-- construct parameter data in the base monad.
paramInit :: Monad m => Constr f m -> ParamT f m (a f) -> m (a f, [Param f])
paramInit f x = runWriterT (runReaderT (PT_R "" f) (unPT x))

pt_iso :: Iso (ReaderT (PT_R f m) (WriterT [Param f] m)) (ParamT f m)
pt_iso = Iso PT unPT

instance Monad m => Functor (ParamT f m) where
  fmap = derive_fmap pt_iso

instance Monad m => Monad (ParamT f m) where
  return = derive_return pt_iso
  (>>=)  = derive_bind   pt_iso

instance Monad m => Applicative (ParamT f m) where
  pure  = return
  (<*>) = ap

instance MonadT (ParamT f) where
  lift = PT . lift . lift

instance Monad m => ReaderM (ParamT f m) (PT_R f m) where
  ask = derive_ask pt_iso

instance Monad m => RunReaderM (ParamT f m) (PT_R f m) where
  local = derive_local pt_iso

instance Monad m => WriterM (ParamT f m) [Param f] where
  put = derive_put pt_iso

-- | Combine parameter names with underscores.
combineNames :: String -> String -> String
combineNames "" b = b
combineNames a  b = a ++ "_" ++ b

-- | Create a parameter given a name and initial value.
param :: Monad m => String -> Float -> ParamT f m (Param f)
param s v = do
  PT_R prefix constr <- ask
  let name = combineNames prefix s
  val <- lift $ constr name v
  let p = Param name v val
  put [p]
  return p

-- | Create a group of parameters prefixed by a name.
group :: Monad m => String -> ParamT f m (a f) -> ParamT f m (a f)
group s g = mapReader (\x -> x {pt_r_prefix = s}) g

