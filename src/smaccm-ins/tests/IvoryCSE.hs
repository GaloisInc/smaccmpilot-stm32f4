{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module IvoryCSE (cse) where

import Control.Applicative
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Reify
import Data.Traversable
import Ivory.Language
import Ivory.Language.Monad
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as AST
import Prelude hiding (foldr)
import System.IO.Unsafe (unsafePerformIO)

newtype MonoidMap k v = MonoidMap { getMap :: Map k v }
  deriving Show

instance (Ord k, Monoid v) => Monoid (MonoidMap k v) where
  mempty = MonoidMap Map.empty
  mappend (MonoidMap a) (MonoidMap b) = MonoidMap $ Map.unionWith mappend a b
  mconcat maps = MonoidMap $ Map.unionsWith mappend [ m | MonoidMap m <- maps ]

data TypeLattice
  = UnknownType
  | HasType AST.Type
  | IllegalType
  deriving Show

instance Monoid TypeLattice where
  mempty = UnknownType
  mappend UnknownType t = t
  mappend IllegalType _ = IllegalType
  mappend t UnknownType = t
  mappend _ IllegalType = IllegalType
  mappend (HasType a) (HasType b) = if a == b then HasType a else IllegalType

data ExprF t
  = ExpSimpleF AST.Expr
    -- ^ For expressions that cannot contain any expressions recursively.
  | ExpLabelF AST.Type t String
  | ExpIndexF AST.Type t AST.Type t
  | ExpToIxF t Integer
  | ExpSafeCastF AST.Type t
  | ExpOpF AST.ExpOp [t]
  deriving (Show, Functor)

instance MuRef AST.Expr where
  type DeRef AST.Expr = ExprF
  mapDeRef _ e@(AST.ExpSym{}) = pure $ ExpSimpleF e
  mapDeRef _ e@(AST.ExpVar{}) = pure $ ExpSimpleF e
  mapDeRef _ e@(AST.ExpLit{}) = pure $ ExpSimpleF e
  mapDeRef child (AST.ExpLabel ty ex nm) = ExpLabelF <$> pure ty <*> child ex <*> pure nm
  mapDeRef child (AST.ExpIndex ty1 ex1 ty2 ex2) = ExpIndexF <$> pure ty1 <*> child ex1 <*> pure ty2 <*> child ex2
  mapDeRef child (AST.ExpToIx ex bound) = ExpToIxF <$> child ex <*> pure bound
  mapDeRef child (AST.ExpSafeCast ty ex) = ExpSafeCastF ty <$> child ex
  mapDeRef child (AST.ExpOp op args) = ExpOpF op <$> traverse child args
  mapDeRef _ e@(AST.ExpAddrOfGlobal{}) = pure $ ExpSimpleF e
  mapDeRef _ e@(AST.ExpMaxMin{}) = pure $ ExpSimpleF e

toExpr :: ExprF AST.Expr -> AST.Expr
toExpr (ExpSimpleF ex) = ex
toExpr (ExpLabelF ty ex nm) = ex `seq` AST.ExpLabel ty ex nm
toExpr (ExpIndexF ty1 ex1 ty2 ex2) = ex1 `seq` ex2 `seq` AST.ExpIndex ty1 ex1 ty2 ex2
toExpr (ExpToIxF ex bound) = ex `seq` AST.ExpToIx ex bound
toExpr (ExpSafeCastF ty ex) = ex `seq` AST.ExpSafeCast ty ex
toExpr (ExpOpF op args) = foldr seq (AST.ExpOp op args) args

useAtType :: Ord k => k -> AST.Type -> MonoidMap k (Sum Int, TypeLattice)
useAtType k ty = MonoidMap $ Map.singleton k (Sum 1, HasType ty)

findUses :: Ord k => AST.Type -> ExprF k -> MonoidMap k (Sum Int, TypeLattice)
findUses _ (ExpSimpleF{}) = mempty
findUses _ (ExpLabelF ty ex _) = ex `useAtType` ty
findUses _ (ExpIndexF ty1 ex1 ty2 ex2) = (ex1 `useAtType` ty1) `mappend` (ex2 `useAtType` ty2)
findUses _ (ExpToIxF ex _) = ex `useAtType` AST.TyInt AST.Int32
findUses _ (ExpSafeCastF ty ex) = ex `useAtType` ty
findUses ty (ExpOpF op args) = mconcat $ case op of
  AST.ExpEq t -> map (`useAtType` t) args
  AST.ExpNeq t -> map (`useAtType` t) args
  AST.ExpCond -> let (cond, rest) = splitAt 1 args in map (`useAtType` AST.TyBool) cond ++ map (`useAtType` ty) rest
  AST.ExpGt _ t -> map (`useAtType` t) args
  AST.ExpLt _ t -> map (`useAtType` t) args
  AST.ExpIsNan t  -> map (`useAtType` t) args
  AST.ExpIsInf t  -> map (`useAtType` t) args
  _ -> map (`useAtType` ty) args

data Annotated k = Annotated !Int !AST.Type !(ExprF k)

annotateTypes :: AST.Type -> Graph ExprF -> Graph Annotated
annotateTypes rootTy (Graph subexprs root) = Graph (snd $ mapAccumL update (root `useAtType` rootTy) subexprs) root
  where
  update types (uniq, ex) = case Map.lookup uniq $ getMap types of
    Just (Sum uses, HasType ty) -> (types `mappend` findUses ty ex, (uniq, Annotated uses ty ex))
    Just _ -> error "IvoryCSE.annotateTypes: common subexpression used at two different types"
    Nothing -> error "IvoryCSE.annotateTypes: cycle detected in expression"

emitSubexpr :: Ord k => (k, Annotated k) -> Map k AST.Expr -> Ivory eff (Map k AST.Expr)
emitSubexpr (ident, Annotated uses ty ex) emitted = do
  let getSubexpr k = let Just v = Map.lookup k emitted in v
  ex' <- case ex of
    ExpSimpleF ex' -> return ex'
    _ | uses < 2 -> return $ toExpr $ fmap getSubexpr ex
    _ -> do
      ex' <- freshVar "cse"
      emit $! AST.Assign ty ex' $! toExpr $ fmap getSubexpr ex
      return $ AST.ExpVar ex'
  return $! Map.insert ident ex' emitted

cseExprF :: Graph Annotated -> Ivory eff AST.Expr
cseExprF (Graph subexprs root) = do
  subexprs' <- foldrM emitSubexpr Map.empty subexprs
  let Just rootExpr = Map.lookup root subexprs'
  return $! rootExpr

cse :: IvoryExpr e => e -> Ivory eff e
cse expr = fmap wrapExpr $! cseExprF $ annotateTypes ty $ unsafePerformIO $ reifyGraph ex
  where
  AST.Typed ty ex = typedExpr expr
