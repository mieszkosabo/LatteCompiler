module Src.CodeGen.GenStmts where

import Control.Monad.Reader
import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Map as M
import Parser.AbsLatte
import Src.CodeGen.GenExpr (emitBinaryOp, genExpr)
import Src.CodeGen.State
import Src.CodeGen.Utils
import qualified Src.Frontend.Types as Types

foldEnv :: (a -> GenM Env) -> [a] -> GenM Env
foldEnv fun l = do
  env <- ask
  foldM f env l
  where
    f = \env stmt -> local (const env) $ fun stmt

genStmts :: [Stmt] -> GenM Env
genStmts = foldEnv genStmt

genStmt :: Stmt -> GenM Env
genStmt (Empty _) = ask
genStmt (SExp _ e) = genExpr e >> ask
genStmt (BStmt _ (Block _ stmts)) = genStmts stmts >> ask
genStmt (Decl _ t items) = foldEnv (declareItem (Types.stripPositionFromType t)) items
genStmt (Ret _ e) = do
  addr <- genExpr e
  emit $ "\tret " ++ addrToLLVMType addr ++ " " ++ show addr
  ask
genStmt (VRet _) = emit "\tret void" >> ask
genStmt (Ass _ (Ident ident) e) = do
  addr <- genExpr e
  setVar ident addr
  ask
genStmt (CondElse _ e thenSt elseSt) = do
  env <- ask
  ifLabel <- freshLabel
  elseLabel <- freshLabel
  finLabel <- freshLabel
  cond <- genExpr e
  emit $ branch cond ifLabel elseLabel
  entryStore <- gets store

  -- if block
  emit $ placeLabel ifLabel
  setLastLabel ifLabel
  genStmt thenSt
  lastIfLabel <- gets lastLabel
  st <- get
  let ifReturns = isExplicitReturn st
  unless ifReturns (emit $ goto finLabel)
  ifStore <- gets store

  -- else Block
  restore entryStore
  emit $ placeLabel elseLabel
  setLastLabel elseLabel
  genStmt elseSt
  lastElseLabel <- gets lastLabel
  st' <- get
  let elseReturns = isExplicitReturn st'
  unless elseReturns (emit $ goto finLabel)
  elseStore <- gets store

  -- final block
  unless
    ifReturns -- in if/else if one branch returns then the other has to as well
    ( do
        emit $ placeLabel finLabel
        restore entryStore
        createPhiNodes finLabel [(lastIfLabel, ifStore), (lastElseLabel, elseStore)]
        setLastLabel finLabel
    )
  return env
genStmt (Cond pos e thenStmt) = do
  env <- ask
  ifLabel <- freshLabel
  finLabel <- freshLabel
  cond <- genExpr e -- TODO: skip branches if cond is `true` or `false`
  emit $ branch cond ifLabel finLabel
  lastEntryLabel <- gets lastLabel
  entryStore <- gets store

  -- if block
  emit $ placeLabel ifLabel
  setLastLabel ifLabel
  genStmt thenStmt
  lastIfLabel <- gets lastLabel
  st <- get
  let ifReturns = isExplicitReturn st
  unless ifReturns (emit $ goto finLabel)
  ifStore <- gets store

  -- final block
  emit $ placeLabel finLabel
  restore entryStore
  unless ifReturns (createPhiNodes finLabel [(lastIfLabel, ifStore), (lastEntryLabel, entryStore)])
  setLastLabel finLabel
  return env
genStmt (Incr _ (Ident ident)) = do
  addr <- getVar ident
  addr' <- emitBinaryOp "add i32" addr (ImmediateInt 1)
  setVar ident addr'
  ask
genStmt (Decr _ (Ident ident)) = do
  addr <- getVar ident
  addr' <- emitBinaryOp "sub i32" addr (ImmediateInt 1)
  setVar ident addr'
  ask
genStmt (While _ e bodyStmt) = do
  env <- ask
  condLabel <- freshLabel
  bodyLabel <- freshLabel
  finLabel <- freshLabel
  lastEntryLabel <- gets lastLabel
  entryStore <- gets store
  emit $ goto condLabel

  -- body block
  emit $ placeLabel bodyLabel
  setLastLabel bodyLabel
  wrapAllAddressesWithLabel condLabel
  genStmt bodyStmt
  lastBodyLabel <- gets lastLabel
  emit $ goto condLabel
  bodyStore <- gets store

  -- cond block
  emit $ placeLabel condLabel
  restore entryStore
  createPhiNodes condLabel [(lastBodyLabel, bodyStore), (lastEntryLabel, entryStore)]
  cond <- genExpr e
  emit $ branch cond bodyLabel finLabel

  -- finalBlock
  emit $ placeLabel finLabel
  setLastLabel finLabel
  return env

declareItem :: Types.LatteType -> Item -> GenM Env
declareItem t (NoInit _ (Ident ident)) = do
  addr <- case t of
    Types.Int -> return $ ImmediateInt 0
    Types.Str -> genAddr Types.Str
    Types.Bool -> return $ ImmediateBool 0
    _ -> undefined
  (_, env) <- declareVar ident addr
  return env
declareItem _ (Init _ (Ident ident) e) = do
  addr <- genExpr e
  (_, env) <- declareVar ident addr
  return env
