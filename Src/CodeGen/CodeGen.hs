module Src.CodeGen.CodeGen where

import Control.Monad.Reader
import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Map as M
import Parser.AbsLatte
import Src.CodeGen.GenStmts
import Src.CodeGen.State
import Src.CodeGen.State (addStringLiteralsDefinitions)
import Src.CodeGen.Utils
import Src.Frontend.Types (stripPositionFromType)
import qualified Src.Frontend.Types as Types
import System.IO

genCode :: [TopDef] -> GenM ()
genCode topdefs = do
  addTopLevelDefs topdefs
  addPredefinedFunctions
  addInternalFunctions
  genCode' topdefs
  st <- get
  addStringLiteralsDefinitions $ reverse $ stringLiterals st

genCode' :: [TopDef] -> GenM ()
genCode' [] = emit ""
genCode' (f : fs) = do
  let (FnDef _ t (Ident ident) args (Block _ stmts)) = f
  let ty = stripPositionFromType t
  (addresses, env) <- addArgsToEnv args
  let types = argsTypes args
  let argString = createArgString types addresses
  emit $ concat ["define ", show ty, " @", ident, "(", argString, ")", "{"]
  entryLabel <- freshLabel
  modify (\s -> s {lastLabel = entryLabel})
  emit $ placeLabel entryLabel
  local (const env) $ genStmts stmts
  st <- get
  when
    (isImplicitReturn st)
    (emit "\tret void")
  emit "}"
  genCode' fs

addFunctionType :: TopDef -> GenM ()
addFunctionType topdef = do
  let (FnDef pos t (Ident ident) args block) = topdef
  let funT = Types.Fun (Types.stripPositionFromType t) (map (\(Arg _ t _) -> Types.stripPositionFromType t) args)
  let (Types.Fun t ts) = funT
  modify (\s -> s {functionTypes = M.insert ident funT (functionTypes s)})

addTopLevelDefs :: [TopDef] -> GenM ()
addTopLevelDefs = mapM_ addFunctionType

addPredefinedFunctions :: GenM ()
addPredefinedFunctions =
  mapM_
    ( \(ident, t) -> do
        modify (\s -> s {functionTypes = M.insert ident t (functionTypes s)})
        let (Types.Fun funType argsTypes) = t
        let argsString = intercalate ", " (map show argsTypes)
        emit $ concat ["declare ", show funType, " @", ident, "(", argsString, ")"]
    )
    Types.predefinedFunctionsTypes

addInternalFunctions :: GenM ()
addInternalFunctions = do
  emit "declare i8* @__concat(i8*, i8*)"