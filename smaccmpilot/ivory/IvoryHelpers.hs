{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module IvoryHelpers where

import Ivory.Language

----------------------------------------------------------------------
-- Ivory Utilities
--
-- If these are generally useful, they should probably be moved into a
-- "user-space" Ivory utility module.

-- | Infix structure field access and dereference.
-- This is a shorthand for 'deref $ s~>x'.
(~>*) :: (IvoryType r, IvoryExpr a, IvoryStruct sym) =>
            Ref (Struct sym) -> Label sym (Stored a) -> Ivory r a
struct ~>* label = deref $ struct~>label
infixl 8 ~>*

-- | handy shorthand for transfering members
resultInto :: (IvoryType r, IvoryExpr a) =>
     Ivory r a -> Ref (Stored a) -> Ivory r ()
resultInto a b = do
  v <- a
  store b v

into :: (IvoryType r, IvoryExpr a) =>
 Ref (Stored a) -> Ref (Stored a) -> Ivory r ()
into a b = do
  v <- deref a
  store b v

-- | Modify the value stored at a reference by a function.
(%=) :: (IvoryType r, IvoryExpr a) => Ref (Stored a) -> (a -> a) -> Ivory r ()
ref %= f = do
  val <- deref ref
  store ref (f val)

-- | Modify the value stored at a reference by a function that returns
-- a value in the Ivory monad.
(%=!) :: (IvoryType r, IvoryExpr a) =>
         Ref (Stored a) -> (a -> Ivory r a) -> Ivory r ()
ref %=! mf = do
  val  <- deref ref
  val' <- mf val
  store ref val'

-- | Increment the value stored at a reference.
(+=) :: (Num a, IvoryType r, IvoryExpr a) => Ref (Stored a) -> a -> Ivory r ()
ref += x = ref %= (+ x)
