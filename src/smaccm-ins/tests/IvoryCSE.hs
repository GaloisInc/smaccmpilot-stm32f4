{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module IvoryCSE (cse) where

import Control.Applicative
import qualified Data.DList as D
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Reify
import Data.Traversable
import Ivory.Language.Proc
import qualified Ivory.Language.Syntax as AST
import MonadLib (WriterT, StateT, Id, get, sets, sets_, put, collect, runM)
import Prelude hiding (foldr, mapM, mapM_)
import System.IO.Unsafe (unsafePerformIO)

data ExprF t
  = ExpSimpleF AST.Expr
    -- ^ For expressions that cannot contain any expressions recursively.
  | ExpLabelF AST.Type t String
  | ExpIndexF AST.Type t AST.Type t
  | ExpToIxF t Integer
  | ExpSafeCastF AST.Type t
  | ExpOpF AST.ExpOp [t]
  deriving (Show, Foldable, Functor, Traversable)

data BlockF t
  = StmtSimple AST.Stmt
    -- ^ For statements that cannot contain any expressions, or that we don't want to CSE.
  | StmtIfTE t t t
  | StmtStore AST.Type AST.Expr t
  | StmtAssign AST.Type AST.Var t
  | Block [t]
  deriving Show

data CSE t
  = CSEExpr (ExprF t)
  | CSEBlock (BlockF t)
  deriving Show

instance MuRef AST.Expr where
  type DeRef AST.Expr = CSE
  mapDeRef child e = CSEExpr <$> case e of
    AST.ExpSym{} -> pure $ ExpSimpleF e
    AST.ExpVar{} -> pure $ ExpSimpleF e
    AST.ExpLit{} -> pure $ ExpSimpleF e
    AST.ExpLabel ty ex nm -> ExpLabelF <$> pure ty <*> child ex <*> pure nm
    AST.ExpIndex ty1 ex1 ty2 ex2 -> ExpIndexF <$> pure ty1 <*> child ex1 <*> pure ty2 <*> child ex2
    AST.ExpToIx ex bound -> ExpToIxF <$> child ex <*> pure bound
    AST.ExpSafeCast ty ex -> ExpSafeCastF ty <$> child ex
    AST.ExpOp op args -> ExpOpF op <$> traverse child args
    AST.ExpAddrOfGlobal{} -> pure $ ExpSimpleF e
    AST.ExpMaxMin{} -> pure $ ExpSimpleF e

instance MuRef AST.Stmt where
  type DeRef AST.Stmt = CSE
  mapDeRef child stmt = CSEBlock <$> case stmt of
    AST.IfTE cond tb fb -> StmtIfTE <$> child cond <*> child tb <*> child fb
    AST.Store ty lhs rhs -> StmtStore ty <$> pure lhs <*> child rhs
    AST.Assign ty var ex -> StmtAssign ty var <$> child ex
    s -> pure $ StmtSimple s

instance (MuRef a, DeRef [a] ~ DeRef a) => MuRef [a] where
  type DeRef [a] = CSE
  mapDeRef child xs = CSEBlock <$> Block <$> traverse child xs

toExpr :: ExprF AST.Expr -> AST.Expr
toExpr (ExpSimpleF ex) = ex
toExpr (ExpLabelF ty ex nm) = AST.ExpLabel ty ex nm
toExpr (ExpIndexF ty1 ex1 ty2 ex2) = AST.ExpIndex ty1 ex1 ty2 ex2
toExpr (ExpToIxF ex bound) = AST.ExpToIx ex bound
toExpr (ExpSafeCastF ty ex) = AST.ExpSafeCast ty ex
toExpr (ExpOpF op args) = AST.ExpOp op args

labelTypes :: AST.Type -> ExprF k -> ExprF (AST.Type, k)
labelTypes _ (ExpSimpleF e) = ExpSimpleF e
labelTypes _ (ExpLabelF ty ex nm) = ExpLabelF ty (ty, ex) nm
labelTypes _ (ExpIndexF ty1 ex1 ty2 ex2) = ExpIndexF ty1 (ty1, ex1) ty2 (ty2, ex2)
labelTypes _ (ExpToIxF ex bd) = ExpToIxF (AST.TyInt AST.Int32, ex) bd
labelTypes _ (ExpSafeCastF ty ex) = ExpSafeCastF ty (ty, ex)
labelTypes ty (ExpOpF op args) = ExpOpF op $ case op of
  AST.ExpEq t -> map ((,) t) args
  AST.ExpNeq t -> map ((,) t) args
  AST.ExpCond -> let (cond, rest) = splitAt 1 args in map ((,) AST.TyBool) cond ++ map ((,) ty) rest
  AST.ExpGt _ t -> map ((,) t) args
  AST.ExpLt _ t -> map ((,) t) args
  AST.ExpIsNan t  -> map ((,) t) args
  AST.ExpIsInf t  -> map ((,) t) args
  _ -> map ((,) ty) args

type Available = Map (AST.Type, Unique) AST.Var

type BlockM a = WriterT (D.DList AST.Stmt) (StateT (Available, Int) Id) a

genBlock :: BlockM () -> BlockM AST.Block
genBlock gen = do
  (avail, _) <- get
  ((), stmts) <- collect gen
  sets_ $ \ (_, maxId) -> (avail, maxId)
  return $ D.toList stmts

type Facts = (Map Unique (ExprF Unique), Map Unique (BlockM ()))

updateFacts :: (Unique, CSE Unique) -> Facts -> Facts
updateFacts (ident, CSEExpr expr) (exprFacts, blockFacts) = (Map.insert ident expr exprFacts, blockFacts)
updateFacts (ident, CSEBlock block) (exprFacts, blockFacts) = updateBlockFacts $ case block of
  StmtSimple s -> put $ D.singleton s
  StmtIfTE exId tId fId -> do
    cond <- emitExpr (AST.TyBool, exId)
    tb <- genBlock $ lookupBlock tId
    fb <- genBlock $ lookupBlock fId
    put $ D.singleton $ AST.IfTE cond tb fb
  StmtAssign ty var exId -> do
    ex <- emitExpr (ty, exId)
    put $ D.singleton $ AST.Assign ty var ex
  StmtStore ty lhs rhsId -> do
    rhs <- emitExpr (ty, rhsId)
    put $ D.singleton $ AST.Store ty lhs rhs
  Block stmts -> mapM_ lookupBlock stmts
  where
  lookupExpr e = let Just facts = Map.lookup e exprFacts in facts
  lookupBlock b = let Just facts = Map.lookup b blockFacts in facts
  updateBlockFacts newFacts = (exprFacts, Map.insert ident newFacts blockFacts)

  emitExpr label@(ty, exId) = case lookupExpr exId of
    ExpSimpleF e -> return e
    ex -> do
      (avail, _) <- get
      case Map.lookup label avail of
        Just var -> return $ AST.ExpVar var
        Nothing -> do
          ex' <- mapM emitExpr $ labelTypes ty ex
          var <- sets $ \ (avail', maxId) ->
            let var = AST.VarName $ "cse" ++ show maxId
            in (var, (Map.insert label var avail', maxId + 1))
          put $ D.singleton $ AST.Assign ty var $ toExpr ex'
          return $ AST.ExpVar var

reconstruct :: Graph CSE -> AST.Block
reconstruct (Graph subexprs root) = D.toList rootBlock
  where
  (_, blockFacts) = foldr updateFacts mempty subexprs
  Just rootGen = Map.lookup root blockFacts
  (((), rootBlock), _finalState) = runM rootGen (Map.empty, 0)

cse :: Def proc -> Def proc
cse (DefProc def) = DefProc def
  { AST.procBody = reconstruct $ unsafePerformIO $ reifyGraph $ AST.procBody def }
cse def = def
