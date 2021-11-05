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
addItemToEnv :: LatteType -> Item -> StaticCheck TEnv
addItemToEnv t item = do
  env <- ask
  checkItem t item
  case item of
    NoInit _ (Ident ident) -> return $ M.insert ident t env
    Init _ (Ident ident) _ -> return $ M.insert ident t env

addItemsToEnv :: LatteType -> TEnv -> [Item] -> StaticCheck TEnv
addItemsToEnv t env items = do
  foldM f env items
  where
    f = \env item -> local (const env) $ addItemToEnv t item

checkItem :: LatteType -> Item -> StaticCheck ()
checkItem _ (NoInit pos (Ident ident)) = do
  env <- ask
  case M.lookup ident env of
    Nothing -> return ()
    Just lt -> throwError $ NameAlreadyExistsInScopeError pos (show ident)
checkItem t (Init pos (Ident ident) e) = do
  env <- ask
  exprType <- evalExprType e
  when (exprType /= t) (throwError $ TypeAssertFailed pos (show t) (show exprType))
  case M.lookup ident env of
    Nothing -> return ()
    Just lt -> throwError $ NameAlreadyExistsInScopeError pos (show ident)

checkStmts :: [Stmt] -> StaticCheck (TEnv, LatteType)
checkStmts stmts = do
  env <- ask
  (env', retTypes, implicitReturn) <- foldM f (env, [], True) stmts
  let filteredRetTypes = if implicitReturn then Types.Void : ts else ts
        where
          ts = nub . catMaybes $ retTypes
  when (length filteredRetTypes > 1) (throwError DifferentReturnTypes) -- TODO add pos info
  return (env', head filteredRetTypes)
  where
    f = \(env, accTypes, implicitReturn) stmt -> do
      (env', retType) <- local (const env) $ checkStmt stmt
      let didReturn = isJust retType
      case stmt of
        Ret ma ex -> return (env', retType : accTypes, False)
        VRet ma -> return (env', Just Types.Void : accTypes, False)
        CondElse ma ex st st' -> return $ if didReturn then (env', retType : accTypes, False) else (env', retType : accTypes, implicitReturn)
        _ -> return (env', retType : accTypes, implicitReturn)

checkStmt :: Stmt -> StaticCheck (TEnv, ReturnedType)
checkStmt (Empty _) = do
  env <- ask
  return (env, Nothing)
checkStmt (BStmt _ (Block _ stmts)) = do
  (_, retType) <- checkStmts stmts
  env <- ask
  return (env, Just retType)
checkStmt (Decl pos t items) = do
  env <- ask
  let latteType = stripPositionFromType t
  env' <- addItemsToEnv latteType env items
  return (env', Nothing)
checkStmt (Ass pos (Ident ident) e) = do
  env <- ask
  let maybeT = M.lookup ident env
  when (isNothing maybeT) (throwError $ UseOfUndeclaredName pos ident)
  let Just t = maybeT
  exprType <- evalExprType e
  when (exprType /= t) (throwError $ TypeAssertFailed pos (show t) (show exprType))
  return (env, Nothing)
checkStmt (Incr pos (Ident ident)) = do
  env <- ask
  case M.lookup ident env of
    Just Types.Int -> return (env, Nothing)
    Nothing -> throwError $ UseOfUndeclaredName pos ident
    Just otherType -> throwError $ TypeAssertFailed pos (show Types.Int) (show otherType)
checkStmt (Decr pos (Ident ident)) = do
  env <- ask
  case M.lookup ident env of
    Just Types.Int -> return (env, Nothing)
    Nothing -> throwError $ UseOfUndeclaredName pos ident
    Just otherType -> throwError $ TypeAssertFailed pos (show Types.Int) (show otherType)
checkStmt (Ret pos e) = do
  t <- evalExprType e
  env <- ask
  return (env, Just t)
checkStmt (VRet pos) = do
  env <- ask
  return (env, Just Types.Void)
checkStmt (Cond pos e stmt) = do
  t <- evalExprType e
  when (t /= Types.Bool) (throwError $ TypeAssertFailed pos (show Types.Bool) (show t))
  (_, retType) <- checkStmt stmt
  env <- ask
  return (env, retType) -- TODO: tutaj źle
checkStmt (CondElse pos e stmt elseStmt) = do
  t <- evalExprType e
  when (t /= Types.Bool) (throwError $ TypeAssertFailed pos (show Types.Bool) (show t))
  (_, retType) <- checkStmt stmt
  (_, retType') <- checkStmt elseStmt
  when (retType /= retType') (throwError $ ReturnTypeVary pos (show retType) (show retType'))
  env <- ask
  return (env, retType)
checkStmt (While pos e stmt) = do
  t <- evalExprType e
  when (t /= Types.Bool) (throwError $ TypeAssertFailed pos (show Types.Bool) (show t))
  (_, retType) <- checkStmt stmt
  env <- ask
  return (env, retType)
checkStmt (SExp pos e) = do
  evalExprType e
  env <- ask
  return (env, Nothing)