module Src.Frontend.CheckStmts where

import Control.Monad.Except
import Control.Monad.Reader
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust, isNothing)
import Maybes (isNothing)
import Parser.AbsLatte as Abs
import Src.Frontend.EvalExprType (evalExprType)
import Src.Frontend.Types as Types

-- TODO: current scope
addItemToEnv :: LocalScope -> LatteType -> Item -> StaticCheck (TEnv, LocalScope)
addItemToEnv localScope t item = do
  env <- ask
  checkItem localScope t item
  case item of
    NoInit _ (Ident ident) -> return (M.insert ident t env, ident : localScope)
    Init _ (Ident ident) _ -> return (M.insert ident t env, ident : localScope)

addItemsToEnv :: LatteType -> TEnv -> LocalScope -> [Item] -> StaticCheck (TEnv, LocalScope)
addItemsToEnv t env localScope items = do
  foldM f (env, localScope) items
  where
    f = \(env, localScope) item -> local (const env) $ addItemToEnv localScope t item

checkItem :: LocalScope -> LatteType -> Item -> StaticCheck ()
checkItem localScope _ (NoInit pos (Ident ident)) = do
  env <- ask
  when
    (ident `elem` localScope)
    (throwError $ NameAlreadyExistsInScopeError pos (show ident))
checkItem localScope t (Init pos (Ident ident) e) = do
  when
    (ident `elem` localScope)
    (throwError $ NameAlreadyExistsInScopeError pos (show ident))
  env <- ask
  exprType <- evalExprType e
  when
    (exprType /= t)
    (throwError $ TypeAssertFailed pos (show t) (show exprType))

checkStmts :: [Stmt] -> StaticCheck (TEnv, [ReturnedType])
checkStmts stmts = do
  env <- ask
  (env', retTypes, lc) <- foldM f (env, [], []) stmts
  let filteredRetTypes = nub $ catMaybes $ map get retTypes
  when
    (length filteredRetTypes > 1)
    (liftIO (print stmts >> print filteredRetTypes) >> throwError DifferentReturnTypes) -- TODO add pos info
  return (env', retTypes)
  where
    f = \(env, accTypes, localScope) stmt -> do
      (env', retType, localScope') <- local (const env) $ checkStmt stmt localScope
      return (env', retType ++ accTypes, localScope')

checkStmt :: Stmt -> LocalScope -> StaticCheck (TEnv, [ReturnedType], LocalScope)
checkStmt (Empty _) ls = do
  env <- ask
  return (env, [Return Nothing], ls)
checkStmt (BStmt _ (Block _ stmts)) ls = do
  (_, retType) <- checkStmts stmts
  env <- ask
  return (env, retType, ls)
checkStmt (Decl pos t items) ls = do
  env <- ask
  let latteType = stripPositionFromType t
  (env', ls') <- addItemsToEnv latteType env ls items
  return (env', [Return Nothing], ls')
checkStmt (Ass pos (Ident ident) e) ls = do
  env <- ask
  let maybeT = M.lookup ident env
  when (isNothing maybeT) (throwError $ UseOfUndeclaredName pos ident)
  let Just t = maybeT
  exprType <- evalExprType e
  when (exprType /= t) (throwError $ TypeAssertFailed pos (show t) (show exprType))
  return (env, [Return Nothing], ls)
checkStmt (Incr pos (Ident ident)) ls = do
  env <- ask
  case M.lookup ident env of
    Just Types.Int -> return (env, [Return Nothing], ls)
    Nothing -> throwError $ UseOfUndeclaredName pos ident
    Just otherType -> throwError $ TypeAssertFailed pos (show Types.Int) (show otherType)
checkStmt (Decr pos (Ident ident)) ls = do
  env <- ask
  case M.lookup ident env of
    Just Types.Int -> return (env, [Return Nothing], ls)
    Nothing -> throwError $ UseOfUndeclaredName pos ident
    Just otherType -> throwError $ TypeAssertFailed pos (show Types.Int) (show otherType)
checkStmt (Ret pos e) ls = do
  t <- evalExprType e
  env <- ask
  return (env, [Return $ Just t], ls)
checkStmt (VRet pos) ls = do
  env <- ask
  return (env, [Return $ Just Types.Void], ls)
checkStmt (Cond pos e stmt) ls = do
  t <- evalExprType e
  when (t /= Types.Bool) (throwError $ TypeAssertFailed pos (show Types.Bool) (show t))
  (_, retType, _) <- checkStmt stmt []
  env <- ask
  return (env, map conditionalReturn retType, ls)
checkStmt (CondElse pos e stmt elseStmt) ls = do
  t <- evalExprType e
  when (t /= Types.Bool) (throwError $ TypeAssertFailed pos (show Types.Bool) (show t))
  (_, retType, _) <- checkStmt stmt []
  (_, retType', _) <- checkStmt elseStmt []
  when
    (retType /= retType')
    (throwError $ ReturnTypeVary pos (show retType) (show retType'))
  env <- ask
  return (env, map conditionalReturn retType, ls)
checkStmt (While pos e stmt) ls = do
  t <- evalExprType e
  when
    (t /= Types.Bool)
    (throwError $ TypeAssertFailed pos (show Types.Bool) (show t))
  (_, retType, _) <- checkStmt stmt []
  env <- ask
  return (env, map conditionalReturn retType, ls)
checkStmt (SExp pos e) ls = do
  evalExprType e
  env <- ask
  return (env, [Return Nothing], ls)