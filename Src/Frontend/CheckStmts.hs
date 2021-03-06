module Src.Frontend.CheckStmts where

import Control.Monad.Except
import Control.Monad.Reader
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust, isNothing)
import Parser.AbsLatte as Abs
import Src.Frontend.EvalExprType (evalExprType, isClassType)
import Src.Frontend.Types as Types

getClassName (Cls n _ _ _) = n
getClassName (StrippedCls n) = n
getClassName _ = undefined

isSuperClass :: LatteType -> LatteType -> StaticCheck Bool
isSuperClass a' b' = do
  if not (isClassType a') || not (isClassType b') then do return False
  else do
    let a = getClassName a'
    let b = getClassName b'
    if a == b then do return True
    else do
      env <- ask
      let (Cls _ _ _ superClassName) = fst $ env M.! b
      case superClassName of
        Nothing -> return False
        (Just s) -> isSuperClass a' (fst $ env M.! s)

addIdentToEnv :: LocalScope -> LatteType -> VarName -> Bool -> StaticCheck (TEnv, LocalScope)
addIdentToEnv localScope t ident isMutable = do
  env <- ask
  return (M.insert ident (t, isMutable) env, ident : localScope)

addItemToEnv :: LocalScope -> LatteType -> Item -> Bool -> StaticCheck (TEnv, LocalScope)
addItemToEnv localScope t item isMutable = do
  checkItem localScope t item
  case item of
    NoInit _ (Ident ident) -> addIdentToEnv localScope t ident isMutable
    Init _ (Ident ident) _ -> addIdentToEnv localScope t ident isMutable

addItemsToEnv :: LatteType -> TEnv -> LocalScope -> [Item] -> [Bool] -> StaticCheck (TEnv, LocalScope)
addItemsToEnv t env localScope items areMutable = do
  foldM f (env, localScope) $ zip items areMutable
  where
    f = \(env, localScope) (item, isMutable) -> local (const env) $ addItemToEnv localScope t item isMutable

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
  isSuperCls <- isSuperClass t exprType
  when
    (exprType /= t && not isSuperCls)
    (throwError $ TypeAssertFailed pos (show t) (show exprType))

normalizeReturnedTypes :: [ReturnedType] -> [LatteType]
normalizeReturnedTypes ts = if null filtered then [Types.Void] else filtered
  where
    filtered = nub $ catMaybes $ map getT ts

checkStmts :: [Stmt] -> StaticCheck (TEnv, [ReturnedType])
checkStmts stmts = do
  env <- ask
  (env', retTypes, lc) <- foldM f (env, [], []) stmts
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
  latteType <- case t of
    (Abs.ClassType _ (Abs.Ident clsName)) -> case M.lookup clsName env of
      Nothing -> throwError $ UseOfUndeclaredName pos clsName
      Just (classT, _) -> return classT
    _ -> return $ stripPositionFromType t
  (env', ls') <- addItemsToEnv latteType env ls items $ repeat True
  return (env', [Return Nothing], ls')
checkStmt (Ass pos lhs rhs) ls = do
  env <- ask
  lhsType <- evalExprType lhs
  rhsType <- evalExprType rhs
  isSuperCls <- isSuperClass lhsType rhsType
  when (rhsType /= lhsType && not isSuperCls) (throwError $ TypeAssertFailed pos (show lhsType) (show rhsType))
  return (env, [Return Nothing], ls)
checkStmt (For pos t (Ident ident) e stmt) ls = do
  env <- ask
  arr <- evalExprType e
  let iteratorT = stripPositionFromType t
  case arr of 
    (Array arrT) -> if arrT /= iteratorT 
      then throwError $ OtherError pos "Type mismatch between iterator type and array type"
      else do
        (env', ls') <- addIdentToEnv ls iteratorT ident True
        local (const env') $ checkStmt stmt ls'
        return (env, [Return Nothing], ls)
    _ -> throwError $ OtherError pos "For loop can be used only with an array"
checkStmt (Incr pos (Ident ident)) ls = do
  env <- ask
  case M.lookup ident env of
    Just (_, False) -> throwError $ FunctionArgumentModification pos
    Just (Types.Int, _) -> return (env, [Return Nothing], ls)
    Nothing -> throwError $ UseOfUndeclaredName pos ident
    Just (otherType, _) -> throwError $ TypeAssertFailed pos (show Types.Int) (show otherType)
checkStmt (Decr pos (Ident ident)) ls = do
  env <- ask
  case M.lookup ident env of
    Just (_, False) -> throwError $ FunctionArgumentModification pos
    Just (Types.Int, _) -> return (env, [Return Nothing], ls)
    Nothing -> throwError $ UseOfUndeclaredName pos ident
    Just (otherType, _) -> throwError $ TypeAssertFailed pos (show Types.Int) (show otherType)
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
  case e of
    ELitTrue _ -> return (env, retType, ls)
    ELitFalse _ -> return (env, [], ls)
    _ -> return (env, map conditionalReturn retType, ls)
checkStmt (CondElse pos e stmt elseStmt) ls = do
  t <- evalExprType e
  when (t /= Types.Bool) (throwError $ TypeAssertFailed pos (show Types.Bool) (show t))
  (_, retType, _) <- checkStmt stmt []
  (_, retType', _) <- checkStmt elseStmt []
  env <- ask
  case e of
    ELitTrue _ -> return (env, retType, ls)
    ELitFalse _ -> return (env, retType', ls)
    _ -> do
      when
        (normalizeReturnedTypes retType /= normalizeReturnedTypes retType')
        (throwError $ ReturnTypeVary pos (show retType) (show retType'))
      return (env, map normalReturn retType, ls)
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