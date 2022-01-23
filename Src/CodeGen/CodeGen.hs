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
import Src.CodeGen.Optimization (lcse, gcse)
import System.Exit (exitSuccess)
import Src.CodeGen.ClassForest (createClassForest, genClassesDefinitions)

genCode :: [TopDef] -> String -> GenM ()
genCode topdefs filename = do
  addPredefinedFunctions filename
  liftIO $ addInternalFunctions filename
  liftIO $ addArrType filename
  genClassesDefinitions filename $ createClassForest topdefs
  addTopLevelDefs fnDefs 
  genCode' fnDefs filename
  st <- get
  liftIO $ addStringLiteralsDefinitions (stringLiterals st) filename
  where
    fnDefs = filter isFnDef topdefs
    isFnDef TopFnDef {} = True
    isFnDef _ = False

genCode' :: [TopDef] -> String -> GenM ()
genCode' [] _ = return ()
genCode' (f : fs) filename = do
  let (TopFnDef _ (FnDef _ t (Ident ident) args (Block _ stmts))) = f 
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
  blks <- gets blocks

  -- optimizations 
  forM_ (M.elems blks) (\b -> do
    b' <- lcse b
    setBlock $ label b'
    modifyBlock b'
    )
  gcse

  blocks <- gets blocks
  forM_ (M.elems blocks) (\b -> do
    liftIO $ appendFile filename $ show b
    )

  liftIO $ appendFile filename "}\n"
  clearBlocks
  genCode' fs filename

addFunctionType :: TopDef -> GenM ()
addFunctionType topdef = do
  let (TopFnDef _ (FnDef _ t (Ident ident) args block)) = topdef 
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
addInternalFunctions filename =
  appendFile filename "declare i8* @__concat(i8*, i8*)\n"

addArrType :: String -> IO ()
addArrType filename = appendFile filename $ intercalate "\n" ["%Arr = type {", "\ti32*,", "\ti32", "}\n"]