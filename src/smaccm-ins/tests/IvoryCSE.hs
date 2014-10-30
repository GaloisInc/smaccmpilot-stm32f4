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

-- | Variable assignments emitted so far.
type Bindings = (Map (AST.Type, Unique) AST.Var, Int)

-- | A monad for emitting both source-level statements as well as
-- assignments that capture common subexpressions.
type BlockM a = WriterT (D.DList AST.Stmt) (StateT Bindings Id) a

-- | We perform CSE on expressions but also across all the blocks in a
-- procedure.
data CSE t
  = CSEExpr (ExprF t)
  | CSEBlock (BlockF t)
  deriving Show

-- | During CSE, we replace recursive references to an expression with a
-- unique ID for that expression.
data ExprF t
  = ExpSimpleF AST.Expr
    -- ^ For expressions that cannot contain any expressions recursively.
  | ExpLabelF AST.Type t String
  | ExpIndexF AST.Type t AST.Type t
  | ExpToIxF t Integer
  | ExpSafeCastF AST.Type t
  | ExpOpF AST.ExpOp [t]
  deriving (Show, Foldable, Functor, Traversable)

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

-- | Convert a flattened expression back to a real expression.
toExpr :: ExprF AST.Expr -> AST.Expr
toExpr (ExpSimpleF ex) = ex
toExpr (ExpLabelF ty ex nm) = AST.ExpLabel ty ex nm
toExpr (ExpIndexF ty1 ex1 ty2 ex2) = AST.ExpIndex ty1 ex1 ty2 ex2
toExpr (ExpToIxF ex bound) = AST.ExpToIx ex bound
toExpr (ExpSafeCastF ty ex) = AST.ExpSafeCast ty ex
toExpr (ExpOpF op args) = AST.ExpOp op args

-- | Label all sub-expressions with the type at which they're used,
-- assuming that this expression is used at the given type.
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

-- | Like ExprF, we replace recursive references to
-- blocks/statements/expressions with unique IDs.
--
-- Note that we treat statements as a kind of block, because extracting
-- assignments for the common subexpressions in a statement can result
-- in multiple statements, which looks much like a block.
--
-- We're not performing CSE on all recursive references yet. For
-- example, extracting common reference-typed expressions from the
-- left-hand side of Store or the right-hand side of Deref generated
-- incorrect code when I tried it. This list can be extended as needed,
-- though.
data BlockF t
  = StmtSimple AST.Stmt
    -- ^ For statements that cannot contain any expressions, or that we don't want to CSE.
  | StmtIfTE t t t
  | StmtStore AST.Type AST.Expr t
  | StmtAssign AST.Type AST.Var t
  | Block [t]
  deriving Show

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

-- | Convert a flattened statement or block back to a real block.
toBlock :: ((AST.Type, k) -> BlockM AST.Expr) -> (k -> BlockM ()) -> BlockF k -> BlockM ()
toBlock expr block b = case b of
  StmtSimple s -> stmt $ return s
  StmtIfTE ex tb fb -> stmt $ AST.IfTE <$> expr (AST.TyBool, ex) <*> genBlock (block tb) <*> genBlock (block fb)
  StmtAssign ty var ex -> stmt $ AST.Assign ty var <$> expr (ty, ex)
  StmtStore ty lhs rhs -> stmt $ AST.Store ty lhs <$> expr (ty, rhs)
  Block stmts -> mapM_ block stmts
  where
  stmt stmtM = fmap D.singleton stmtM >>= put

-- | When a statement contains a block, we need to propagate the
-- available expressions into that block. However, on exit from that
-- block, the expressions it made newly-available go out of scope, so we
-- remove them from the available set for subsequent statements.
genBlock :: BlockM () -> BlockM AST.Block
genBlock gen = do
  (avail, _) <- get
  ((), stmts) <- collect gen
  sets_ $ \ (_, maxId) -> (avail, maxId)
  return $ D.toList stmts

-- | Data to accumulate as we analyze each expression and each
-- block/statement.
type Facts = (Map Unique (ExprF Unique), Map Unique (BlockM ()))

-- | Walk a reified AST in topo-sorted order, accumulating analysis
-- results.
updateFacts :: (Unique, CSE Unique) -> Facts -> Facts
updateFacts (ident, CSEExpr expr) (exprFacts, blockFacts) = (Map.insert ident expr exprFacts, blockFacts)
updateFacts (ident, CSEBlock block) (exprFacts, blockFacts) = (exprFacts, Map.insert ident (toBlock emitExpr lookupBlock block) blockFacts)
  where
  lookupBlock b = let Just facts = Map.lookup b blockFacts in facts
  emitExpr label@(ty, exId) = case Map.lookup exId exprFacts of
    Nothing -> error "IvoryCSE.emitExpr: infinite cycle in expression graph"
    Just (ExpSimpleF e) -> return e
    Just ex -> do
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

-- | Given a reified AST, reconstruct an Ivory AST with all sharing made
-- explicit.
reconstruct :: Graph CSE -> AST.Block
reconstruct (Graph subexprs root) = D.toList rootBlock
  where
  (_, blockFacts) = foldr updateFacts mempty subexprs
  Just rootGen = Map.lookup root blockFacts
  (((), rootBlock), _finalState) = runM rootGen (Map.empty, 0)

-- | Find each common sub-expression and extract it to a new variable,
-- making any sharing explicit. However, this function should never move
-- evaluation of an expression earlier than it would have occurred in
-- the source program, which means that sometimes an expression must be
-- re-computed on each of several execution paths.
cse :: Def proc -> Def proc
cse (DefProc def) = DefProc def
  { AST.procBody = reconstruct $ unsafePerformIO $ reifyGraph $ AST.procBody def }
cse def = def
