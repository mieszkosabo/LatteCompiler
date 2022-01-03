module Src.CodeGen.CodeGen where

import Control.Monad.Reader
import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Map as M
import Parser.AbsLatte
import Src.CodeGen.GenStmts
import Src.CodeGen.State
import Src.CodeGen.Utils
import Src.Frontend.Types (stripPositionFromType)
import qualified Src.Frontend.Types as Types
import System.IO

genCode :: [TopDef] -> String -> GenM ()
genCode topdefs filename = do
  addTopLevelDefs topdefs 
  addPredefinedFunctions filename
  liftIO $ addInternalFunctions filename
  genCode' topdefs filename
  st <- get
  liftIO $ addStringLiteralsDefinitions (stringLiterals st) filename

genCode' :: [TopDef] -> String -> GenM ()
genCode' [] _ = return ()
genCode' (f : fs) filename = do
  let (FnDef _ t (Ident ident) args (Block _ stmts)) = f
  let ty = stripPositionFromType t
  (addresses, env) <- addArgsToEnv args
  let types = argsTypes args
  let argString = createArgString types addresses
  liftIO $ appendFile filename $ concat ["define ", show ty, " @", ident, "(", argString, ")", "{\n"]
  
  l <- addBlock []
  setBlock l
  emit $ placeLabel l
  local (const env) $ genStmts stmts
  st <- get
  when
    (isImplicitReturn st)
    (emit (Nothing, IVRet))
  blocks <- gets blocks
  forM_ (M.elems blocks) (liftIO . appendFile filename . show)

  liftIO $ appendFile filename "}\n"
  clearBlocks
  genCode' fs filename

addFunctionType :: TopDef -> GenM ()
addFunctionType topdef = do
  let (FnDef pos t (Ident ident) args block) = topdef
  let funT = Types.Fun (Types.stripPositionFromType t) (map (\(Arg _ t _) -> Types.stripPositionFromType t) args)
  let (Types.Fun t ts) = funT
  modify (\s -> s {functionTypes = M.insert ident funT (functionTypes s)})

addTopLevelDefs :: [TopDef] -> GenM ()
addTopLevelDefs = mapM_ addFunctionType

addPredefinedFunctions :: String -> GenM ()
addPredefinedFunctions filename =
  mapM_
    ( \(ident, t) -> do
        modify (\s -> s {functionTypes = M.insert ident t (functionTypes s)})
        let (Types.Fun funType argsTypes) = t
        let argsString = intercalate ", " (map show argsTypes)
        liftIO $ appendFile filename $ concat ["declare ", show funType, " @", ident, "(", argsString, ")\n"]
    )
    Types.predefinedFunctionsTypes

addInternalFunctions :: String -> IO ()
addInternalFunctions filename = do
  appendFile filename "declare i8* @__concat(i8*, i8*)\n"