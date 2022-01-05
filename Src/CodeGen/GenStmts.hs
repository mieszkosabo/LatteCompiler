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
  emit (Nothing, IRet (addrToLLVMType addr) addr)
  ask
genStmt (VRet _) = emit (Nothing, IVRet) >> ask
genStmt (Ass _ (Ident ident) e) = do
  addr <- genExpr e
  setVar ident addr
  ask
genStmt (CondElse _ e thenSt elseSt) = do
  cond <- genExpr e
  case cond of
    ImmediateBool 1 -> do
      -- `if(true)` then just generate `thenStmt` w/o branching
      genStmt thenSt
      ask
    ImmediateBool 0 -> do
      -- `if(false)` then just generate `elseStmt` w/o branching
      genStmt elseSt
      ask
    _ -> do
      l <- gets currentBlock
      ifLabel <- addBlock [l]
      elseLabel <- addBlock [l]
      finLabel <- addBlock [ifLabel, elseLabel]
      emit $ branch cond ifLabel elseLabel
      entryStore <- gets store

      -- if block
      setBlock ifLabel
      emit $ placeLabel ifLabel
      genStmt thenSt
      lastIfLabel <- gets currentBlock
      st <- get
      let ifReturns = isExplicitReturn st
      unless ifReturns (emit $ goto finLabel)
      ifStore <- gets store

      -- else Block
      restore entryStore
      setBlock elseLabel
      emit $ placeLabel elseLabel
      genStmt elseSt
      lastElseLabel <- gets currentBlock
      st' <- get
      let elseReturns = isExplicitReturn st'
      unless elseReturns (emit $ goto finLabel)
      elseStore <- gets store

      -- final block
      unless
        ifReturns -- in if/else if one branch returns then the other has to as well
        ( do
            emit $ placeLabel finLabel
            setBlock finLabel
            restore entryStore
            createPhiNodes finLabel [(lastIfLabel, ifStore), (lastElseLabel, elseStore)]
        )
      ask
genStmt (Cond pos e thenStmt) = do
  cond <- genExpr e
  case cond of
    ImmediateBool 1 -> do
      -- `if(true)` then just generate stmt w/o branching
      genStmt thenStmt
      ask
    ImmediateBool 0 -> ask -- `if(false)` then just skip the stmt inside
    _ -> do
      l <- gets currentBlock
      ifLabel <- addBlock [l]
      finLabel <- addBlock [l, ifLabel]
      emit $ branch cond ifLabel finLabel
      entryStore <- gets store

      -- if block
      setBlock ifLabel
      emit $ placeLabel ifLabel
      genStmt thenStmt
      lastIfLabel <- gets currentBlock
      st <- get
      let ifReturns = isExplicitReturn st
      unless ifReturns (emit $ goto finLabel)
      ifStore <- gets store

      -- final block
      setBlock finLabel
      emit $ placeLabel finLabel
      restore entryStore
      unless ifReturns (createPhiNodes finLabel [(lastIfLabel, ifStore), (l, entryStore)])
      ask
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
  l <- gets currentBlock
  condLabel <- addBlock [l]
  bodyLabel <- addBlock [condLabel]
  finLabel <- addBlock [condLabel]
  lastEntryLabel <- gets currentBlock
  entryStore <- gets store
  emit $ goto condLabel

  -- body block
  setBlock bodyLabel
  emit $ placeLabel bodyLabel
  wrapAllAddressesWithLabel condLabel
  genStmt bodyStmt
  lastBodyLabel <- gets currentBlock
  emit $ goto condLabel
  bodyStore <- gets store

  -- cond block
  setBlock condLabel
  emit $ placeLabel condLabel
  restore entryStore
  createPhiNodes condLabel [(lastBodyLabel, bodyStore), (lastEntryLabel, entryStore)]
  cond <- genExpr e
  emit $ branch cond bodyLabel finLabel

  -- finalBlock
  setBlock finLabel
  emit $ placeLabel finLabel
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
