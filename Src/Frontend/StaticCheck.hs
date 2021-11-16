module Src.Frontend.StaticCheck where

import Control.Monad.Except
import Control.Monad.Reader
import Data.List (nub)
import qualified Data.Map as M
import Maybes (catMaybes, isNothing)
import Parser.AbsLatte as Abs
import Src.Frontend.CheckStmts (checkStmts, normalizeReturnedTypes)
import Src.Frontend.Types as Types

checkProgram :: Program -> StaticCheck ()
checkProgram program = do
  let (Program _ topdefs) = program
  env <- addTopLevelDefs topdefs
  let env' = addPredefinedFunctions env
  checkIfMainFunctionExits env'
  checkFunctions env' topdefs
  return ()

runStaticCheck :: Program -> IO (Either TypeCheckErrors ())
runStaticCheck program = runExceptT $ runReaderT (checkProgram program) M.empty

-- add all top level funcs to env, their names must be unique
addTopLevelDef :: TopDef -> StaticCheck TEnv
addTopLevelDef topdef = do
  let (FnDef pos t (Ident ident) args block) = topdef
  when
    (ident == "main")
    (checkMainFunction topdef)
  env <- ask
  case M.lookup ident env of
    Nothing -> do
      let funT = Types.Fun (Types.stripPositionFromType t) (map (\(Arg _ t _) -> Types.stripPositionFromType t) args)
      let env' = M.insert ident (funT, False) env
      return env'
    Just _ -> throwError (NameAlreadyExistsInScopeError pos ident)

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

addFunctionArgumentsToEnv :: TEnv -> [Arg] -> TEnv
addFunctionArgumentsToEnv =
  foldr
    ( (\(ident, t) env -> M.insert ident (t, True) env)
        . (\(Arg _ t (Ident ident)) -> (ident, stripPositionFromType t))
    )

-- check if specifications that are special to main func are matched:
-- 1. int return type
-- 2. no args
checkMainFunction :: TopDef -> StaticCheck ()
checkMainFunction (FnDef pos t _ args block) = do
  unless
    (Prelude.null args)
    (throwError $ MainFunctionTakesArgumentsError pos)

  case t of
    Abs.Int _ -> return ()
    _ -> throwError $ MainFunctionMustReturnInt pos

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
checkSingleFunction env (FnDef pos t (Ident ident) args (Block _ stmts)) = do
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
  when
    (stripPositionFromType t /= head retTypesWithoutDuplicates)
    (throwError $ MainFunctionMustReturnInt pos)

checkFunctions :: TEnv -> [TopDef] -> StaticCheck ()
checkFunctions env topdefs = forM_ topdefs (checkSingleFunction env)
