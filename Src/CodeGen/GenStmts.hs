module Src.CodeGen.GenStmts where

import Control.Monad.Reader
import Control.Monad.State
import Parser.AbsLatte
import Src.CodeGen.GenExpr (genExpr)
import Src.CodeGen.State

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

declareItem :: Item -> GenM Env
declareItem (NoInit _ (Ident ident)) = do
  (_, env) <- declareVar ident (Left 0)
  return env
declareItem (Init _ (Ident ident) e) = do
  addr <- genExpr e
  (_, env) <- declareVar ident (Right addr)
  return env
