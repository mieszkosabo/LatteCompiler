module Src.Frontend.StaticCheck where

import Control.Monad.Except
import Control.Monad.Reader
import Data.List (nub, find)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isNothing)
import Parser.AbsLatte as Abs
import Src.Frontend.CheckStmts (checkStmts, normalizeReturnedTypes)
import Src.Frontend.Types as Types

checkProgram :: Program -> StaticCheck ()
checkProgram program = do
  let (Program _ topdefs) = program
  env <- addTopLevelDefs topdefs
  let env' = addPredefinedFunctions env
  checkIfMainFunctionExits env'
  env'' <- checkClasses env' topdefs
  checkFunctions env'' topdefs
  return ()

runStaticCheck :: Program -> IO (Either TypeCheckErrors ())
runStaticCheck program = runExceptT $ runReaderT (checkProgram program) M.empty

-- add all top level funcs to env, their names must be unique
addTopLevelDef :: TopDef -> StaticCheck TEnv
addTopLevelDef topdef@(TopFnDef pos fnDef@(FnDef _ t (Ident ident) args block)) = do
  when
    (ident == "main")
    (checkMainFunction topdef)
  env <- ask
  case M.lookup ident env of
    Nothing -> do
      let funT = getFnType fnDef
      return $ M.insert ident (funT, False) env
    Just _ -> throwError (NameAlreadyExistsInScopeError pos ident)
addTopLevelDef (ClassDef pos (Ident clsName) clsStmts) = do
  env <- ask
  case M.lookup clsName env of
    Just _ -> throwError (NameAlreadyExistsInScopeError pos clsName)
    Nothing -> do
      let classT = Types.Cls clsName (getMethodsTypes clsStmts) (getAttributesTypes clsStmts) Nothing
      return $ M.insert clsName (classT, False) env
addTopLevelDef (ClassExtDef pos (Ident clsName) (Ident superClsName) clsStmts) = do
  env <- ask
  case M.lookup clsName env of
    Just _ -> throwError (NameAlreadyExistsInScopeError pos clsName)
    Nothing -> do
      let classT = Types.StrippedCls clsName
      return $ M.insert clsName (classT, False) env

getFnType :: FnDef -> LatteType
getFnType (FnDef _ t (Ident ident) args block) = Types.Fun (Types.stripPositionFromType t) (map (\(Arg _ t _) -> Types.stripPositionFromType t) args)

getAttributesTypes :: [ClassStmt] -> [(String, LatteType)]
getAttributesTypes classStmts = map (\(AttrProp _ t (Ident ident)) -> (ident, Types.stripPositionFromType t)) (filter (not . isFnProp) classStmts)

isFnProp FnProp {} = True
isFnProp _ = False

getMethodsTypes :: [ClassStmt] -> [(String, LatteType)]
getMethodsTypes classStmts = map (\(FnProp _ fnDef@(FnDef _ t (Ident ident) args block)) -> (ident, getFnType fnDef)) (filter isFnProp classStmts)

addTopLevelDefs topdefs = do
  env <- ask
  foldM f env topdefs
  where
    f = \env topdef -> local (const env) $ addTopLevelDef topdef

addPredefinedFunctions :: TEnv -> TEnv
addPredefinedFunctions env =
  foldr
    (\(ident, t) env -> M.insert ident (t, False) env)
    env
    Types.predefinedFunctionsTypes

addVarsToEnv :: TEnv -> [(String, LatteType)] -> TEnv
addVarsToEnv
  = foldr (\ (ident, t) env -> M.insert ident (t, True) env)

addFunctionArgumentsToEnv :: TEnv -> [Arg] -> TEnv
addFunctionArgumentsToEnv env args = addVarsToEnv env (map (\(Arg _ t (Ident ident)) -> (ident, stripPositionFromType t)) args)

-- check if specifications that are special to main func are matched:
-- 1. int return type
-- 2. no args
checkMainFunction :: TopDef -> StaticCheck ()
checkMainFunction (TopFnDef pos (FnDef _ t _ args block)) = do
  unless
    (Prelude.null args)
    (throwError $ MainFunctionTakesArgumentsError pos)

  case t of
    Abs.Int _ -> return ()
    _ -> throwError $ MainFunctionMustReturnInt pos
checkMainFunction _ = undefined

checkIfMainFunctionExits :: TEnv -> StaticCheck ()
checkIfMainFunctionExits env = do
  let mainFn = M.lookup "main" env
  when
    (isNothing mainFn)
    (throwError NoMainFunctionError)

checkIfFunctionArgsHaveUniqueNames :: [Arg] -> Bool
checkIfFunctionArgsHaveUniqueNames args = length names == length namesWithoutDuplicates
  where
    names = Prelude.map (\(Arg _ _ (Ident name)) -> name) args
    namesWithoutDuplicates = nub names

checkSingleFunction :: TEnv -> TopDef -> StaticCheck ()
checkSingleFunction env (TopFnDef pos (FnDef _ t (Ident ident) args (Block _ stmts))) = do
  unless
    (checkIfFunctionArgsHaveUniqueNames args)
    (throwError $ OtherError pos "Function arguments don't have unique names")
  let envWithFunctionArgs = addFunctionArgumentsToEnv env args
  (_, retTypes) <- local (const envWithFunctionArgs) (checkStmts stmts)

  let retTypesWithoutDuplicates = normalizeReturnedTypes ts
        where
          ts = if all isConditionalReturn retTypes then Return (Just Types.Void) : retTypes else retTypes
  when
    (length retTypesWithoutDuplicates /= 1)
    (throwError $ DifferentReturnTypes pos)
  latteType <- case t of
    (Abs.ClassType _ (Abs.Ident clsName)) -> case M.lookup clsName env of
      Nothing -> throwError $ UseOfUndeclaredName pos clsName
      Just (classT, _) -> return classT
    _ -> return $ stripPositionFromType t
  when
    (latteType /= head retTypesWithoutDuplicates)
    (throwError $ ReturnTypeVary pos (show $ stripPositionFromType t) (show $ head retTypesWithoutDuplicates))
checkSingleFunction env _ = undefined 

checkFunctions :: TEnv -> [TopDef] -> StaticCheck ()
checkFunctions env topdefs = forM_ (filter isFunction topdefs) (checkSingleFunction env)

checkSingleClass :: TEnv -> TopDef -> StaticCheck TEnv
checkSingleClass env cls = do
  let (pos, clsName, clsStmts, superClassName) = case cls of
        (ClassDef pos (Ident clsName) clsStmts) -> (pos, clsName, clsStmts, Nothing)
        (ClassExtDef pos (Ident clsName) (Ident superClassName) clsStmts) -> (pos, clsName, clsStmts, Just superClassName)
        _ -> undefined
  let classT = Types.Cls clsName (getMethodsTypes clsStmts) (getAttributesTypes clsStmts) superClassName
  let (Cls _ methodsTypes attrs _) = classT
  let envWithAttributes = addVarsToEnv env attrs
  let methods = filter isFnProp clsStmts
  envWithMethods <- foldM (\e (FnProp _ fnDef@(FnDef _ _ (Ident ident) _ _)) ->
    local (const e) (do 
      let funT = getFnType fnDef
      return $ M.insert ident (funT, False) e)
    ) envWithAttributes methods
  let envWithSelf = addVarsToEnv envWithMethods [("self", classT)]
  forM_ methods (\(FnProp _ (FnDef _ _ (Ident methodName) args (Block _ stmts))) -> do
    let envWithFunctionArgs = addFunctionArgumentsToEnv envWithSelf args
    (_, retTypes) <- local (const envWithFunctionArgs) (checkStmts stmts)
    let retTypesWithoutDuplicates = normalizeReturnedTypes ts
          where
          ts = if all isConditionalReturn retTypes then Return (Just Types.Void) : retTypes else retTypes
    when
      (length retTypesWithoutDuplicates /= 1)
      (throwError $ DifferentReturnTypes pos)
    let (Just (_, Types.Fun t _)) = find (\(n, _) -> n == methodName) methodsTypes
    when
      (t /= head retTypesWithoutDuplicates)
      (throwError $ ReturnTypeVary pos (show t) (show $ head retTypesWithoutDuplicates))
    )
  return $ M.insert clsName (classT, False) env


checkClasses :: TEnv -> [TopDef] -> StaticCheck TEnv
checkClasses env topdefs = foldM (\en cls -> local (const en) $ checkSingleClass en cls) env (filter (not . isFunction) topdefs)

isFunction :: TopDef -> Bool
isFunction TopFnDef {} = True
isFunction _ = False
