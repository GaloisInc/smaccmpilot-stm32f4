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
(~>*) :: (IvoryVar a, IvoryStruct sym, IvoryDeref ref,
          IvoryExpr (ref s (Stored a)),
          IvoryExpr (ref s (Struct sym))) =>
            ref s (Struct sym) -> Label sym (Stored a) -> Ivory lex r a
struct ~>* label = deref $ struct~>label
infixl 8 ~>*

-- | handy shorthand for transfering members
resultInto :: IvoryStore ref a =>
     Ivory lex r a -> Ref ref (Stored a) -> Ivory lex r ()
resultInto a b = store b =<< a

into :: IvoryStore ref' a =>
     Ref ref (Stored a) -> Ref ref' (Stored a) -> Ivory lex r ()
into a b = store b =<< deref a

-- | Modify the value stored at a reference by a function.
(%=) :: IvoryStore ref a =>
     Ref ref (Stored a) -> (a -> a) -> Ivory lex r ()
ref %= f = do
  val <- deref ref
  store ref (f val)

-- | Modify the value stored at a reference by a function that returns
-- a value in the Ivory monad.
(%=!) :: IvoryStore ref a =>
         Ref ref (Stored a) -> (a -> Ivory lex r a) -> Ivory lex r ()
ref %=! mf = do
  val  <- deref ref
  val' <- mf val
  store ref val'

-- | Increment the value stored at a reference.
(+=) :: (Num a, IvoryStore ref a) =>
        Ref ref (Stored a) -> a -> Ivory lex r ()
ref += x = ref %= (+ x)

ift :: IBool -> Ivory (Block s) r a -> Ivory s r ()
ift p c = ifte p c (return ())

