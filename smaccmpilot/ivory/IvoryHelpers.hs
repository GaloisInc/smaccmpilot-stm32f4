{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module IvoryHelpers where

import Ivory.Language

-- I would like to annotate this with a type signature but Label is not
-- exported from Ivory.Language
-- (~>*) :: (IvoryType r, IvoryExpr a) =>
--     Ref (Struct sym) -> Label sym (Stored a) -> Ivory r a
-- This is a shorthand for 'deref $ s~>x'.
struct ~>* label = deref $ struct~>label
infixl 8 ~>*

-- Another handy shorthand for transfering members
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

