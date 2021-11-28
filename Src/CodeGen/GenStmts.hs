{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Src.CodeGen.GenStmts where

import Control.Monad.Reader
import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Map as M
import Parser.AbsLatte
import Src.CodeGen.GenExpr (emitBinaryOp, genExpr)
import Src.CodeGen.State
import Src.CodeGen.Utils

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
genStmt (BStmt _ (Block _ stmts)) = genStmts stmts
genStmt (Decl _ t items) = foldEnv declareItem items -- TODO: pass type so that the initial value can be correctly inited
genStmt (Ret _ e) = do
  addr <- genExpr e
  emit $ "\tret i32 " ++ show addr
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
  modify (\s -> s {lastLabel = ifLabel})
  genStmt thenSt
  lastIfLabel <- gets lastLabel
  emit $ goto finLabel
  ifStore <- gets store

  -- else Block
  modify (\s -> s {store = entryStore}) -- restore entry state before going into elseStmt
  emit $ placeLabel elseLabel
  modify (\s -> s {lastLabel = elseLabel})
  genStmt elseSt
  lastElseLabel <- gets lastLabel
  emit $ goto finLabel
  elseStore <- gets store

  -- final block
  emit $ placeLabel finLabel
  modify (\s -> s {store = entryStore})
  createPhiNodes finLabel [(lastIfLabel, ifStore), (lastElseLabel, elseStore)]
  modify (\s -> s {lastLabel = finLabel})
  return env
genStmt (Cond pos e thenStmt) = genStmt (CondElse pos e thenStmt (Empty pos)) -- `if` is like `if else` just with empty else
genStmt (Incr _ (Ident ident)) = do
  addr <- getVar ident
  addr' <- emitBinaryOp "add i32" addr (Literal 1)
  setVar ident addr'
  ask
genStmt (Decr _ (Ident ident)) = do
  addr <- getVar ident
  addr' <- emitBinaryOp "sub i32" addr (Literal 1)
  setVar ident addr'
  ask

-- TODO: While, wrap all addresses in body store withLabel
genStmt (While _ e bodyStmt) = do
  env <- ask
  condLabel <- freshLabel
  bodyLabel <- freshLabel
  finLabel <- freshLabel
  lastEntryLabel <- gets lastLabel
  emit $ goto condLabel
  entryStore <- gets store

  emit $ placeLabel bodyLabel
  modify (\s -> s {lastLabel = bodyLabel})
  -- rename vars
  forM_
    (M.keys env)
    ( \varname -> do
        a <- getVar varname
        setVar varname (WithLabel varname condLabel a)
    )
  genStmt bodyStmt
  lastBodyLabel <- gets lastLabel
  emit $ goto condLabel
  bodyStore <- gets store

  emit $ placeLabel condLabel
  modify (\s -> s {store = entryStore})
  createPhiNodes condLabel [(lastBodyLabel, bodyStore), (lastEntryLabel, entryStore)]
  cond <- genExpr e
  emit $ branch cond bodyLabel finLabel

  emit $ placeLabel finLabel
  modify (\s -> s {lastLabel = finLabel})
  return env

declareItem :: Item -> GenM Env
declareItem (NoInit _ (Ident ident)) = do
  (_, env) <- declareVar ident (Left 0)
  return env
declareItem (Init _ (Ident ident) e) = do
  addr <- genExpr e
  (_, env) <- declareVar ident (Right addr)
  return env
