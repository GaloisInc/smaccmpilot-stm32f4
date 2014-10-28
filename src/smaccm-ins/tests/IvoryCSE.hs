{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveFoldable #-}
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

data ExprF t
  = ExpSimpleF AST.Expr
    -- ^ For expressions that cannot contain any expressions recursively.
  | ExpLabelF AST.Type t String
  | ExpIndexF AST.Type t AST.Type t
  | ExpToIxF t Integer
  | ExpSafeCastF AST.Type t
  | ExpOpF AST.ExpOp [t]
  deriving (Show, Foldable, Functor)

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

labelTypes :: ExprF k -> AST.Type -> ExprF (AST.Type, k)
labelTypes (ExpSimpleF e) _ = ExpSimpleF e
labelTypes (ExpLabelF ty ex nm) _ = ExpLabelF ty (ty, ex) nm
labelTypes (ExpIndexF ty1 ex1 ty2 ex2) _ = ExpIndexF ty1 (ty1, ex1) ty2 (ty2, ex2)
labelTypes (ExpToIxF ex bd) _ = ExpToIxF (AST.TyInt AST.Int32, ex) bd
labelTypes (ExpSafeCastF ty ex) _ = ExpSafeCastF ty (ty, ex)
labelTypes (ExpOpF op args) ty = ExpOpF op $ case op of
  AST.ExpEq t -> map ((,) t) args
  AST.ExpNeq t -> map ((,) t) args
  AST.ExpCond -> let (cond, rest) = splitAt 1 args in map ((,) AST.TyBool) cond ++ map ((,) ty) rest
  AST.ExpGt _ t -> map ((,) t) args
  AST.ExpLt _ t -> map ((,) t) args
  AST.ExpIsNan t  -> map ((,) t) args
  AST.ExpIsInf t  -> map ((,) t) args
  _ -> map ((,) ty) args

data Annotated k = Annotated !(Map AST.Type Int) !(ExprF k)

annotateTypes :: AST.Type -> Graph ExprF -> Graph Annotated
annotateTypes rootTy (Graph subexprs root) = Graph (snd $ mapAccumL update (useAtType rootTy root) subexprs) root
  where
  useAtType ty k = MonoidMap $ Map.singleton k $ MonoidMap $ Map.singleton ty (Sum 1)
  update useMap (uniq, ex) = case Map.lookup uniq $ getMap useMap of
    Just (MonoidMap typeMap) -> (useMap `mappend` foldMap (foldMap (uncurry useAtType) . labelTypes ex) (Map.keys typeMap), (uniq, Annotated (fmap getSum typeMap) ex))
    Nothing -> error "IvoryCSE.annotateTypes: cycle detected in expression"

emitSubexpr :: Ord k => (k, Annotated k) -> Map (AST.Type, k) AST.Expr -> Ivory eff (Map (AST.Type, k) AST.Expr)
emitSubexpr (ident, Annotated typeMap ex) earlier = foldrM atType earlier $ Map.toList typeMap
  where
  getSubexpr k = let Just v = Map.lookup k earlier in v
  atType (ty, uses) emitted = do
    ex' <- case ex of
      ExpSimpleF ex' -> return ex'
      _ | uses < 2 -> return $ toExpr $ fmap getSubexpr $ labelTypes ex ty
      _ -> do
        ex' <- freshVar "cse"
        emit $! AST.Assign ty ex' $! toExpr $ fmap getSubexpr $ labelTypes ex ty
        return $ AST.ExpVar ex'
    return $! Map.insert (ty, ident) ex' emitted

cseExprF :: AST.Type -> Graph Annotated -> Ivory eff AST.Expr
cseExprF rootTy (Graph subexprs root) = do
  subexprs' <- foldrM emitSubexpr Map.empty subexprs
  let Just rootExpr = Map.lookup (rootTy, root) subexprs'
  return $! rootExpr

cse :: IvoryExpr e => e -> Ivory eff e
cse expr = fmap wrapExpr $! cseExprF ty $ annotateTypes ty $ unsafePerformIO $ reifyGraph ex
  where
  AST.Typed ty ex = typedExpr expr
