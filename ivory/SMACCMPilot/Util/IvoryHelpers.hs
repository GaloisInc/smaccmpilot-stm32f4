{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Util.IvoryHelpers where

import Ivory.Language

----------------------------------------------------------------------
-- Ivory Utilities
--
-- If these are generally useful, they should probably be moved into a
-- "user-space" Ivory utility module.

-- | Infix structure field access and dereference.
-- This is a shorthand for 'deref $ s~>x'.
(~>*) :: (IvoryVar a, IvoryStruct sym, IvoryRef ref,
          IvoryExpr (ref s (Stored a)),
          IvoryExpr (ref s (Struct sym))) =>
            ref s (Struct sym) -> Label sym (Stored a) -> Ivory eff a
struct ~>* label = deref $ struct~>label
infixl 8 ~>*

-- | handy shorthand for transfering members
resultInto :: IvoryStore a =>
     Ivory eff a -> Ref s (Stored a) -> Ivory eff ()
resultInto a b = store b =<< a

into :: IvoryStore a =>
     Ref s (Stored a) -> Ref s' (Stored a) -> Ivory eff ()
into a b = store b =<< deref a

-- | Modify the value stored at a reference by a function.
(%=) :: IvoryStore a =>
     Ref s (Stored a) -> (a -> a) -> Ivory eff ()
ref %= f = do
  val <- deref ref
  store ref (f val)

-- | Modify the value stored at a reference by a function that returns
-- a value in the Ivory monad.
(%=!) :: IvoryStore a =>
         Ref s (Stored a) -> (a -> Ivory eff a) -> Ivory eff ()
ref %=! mf = do
  val  <- deref ref
  val' <- mf val
  store ref val'

-- | Increment the value stored at a reference.
(+=) :: (Num a, IvoryStore a) =>
        Ref s (Stored a) -> a -> Ivory eff ()
ref += x = ref %= (+ x)

ift :: IBool -> Ivory eff a -> Ivory eff ()
ift p c = ifte p c (return ())

