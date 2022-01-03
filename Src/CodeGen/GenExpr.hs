module Src.CodeGen.GenExpr where

import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Map as M
import Parser.AbsLatte
import Src.CodeGen.State
import Src.CodeGen.Utils
import qualified Src.Frontend.Types as Types

genExpr :: Expr -> GenM Address
genExpr (ELitInt _ n) = return $ ImmediateInt $ fromInteger n
genExpr (EVar _ (Ident ident)) = getVar ident
genExpr (ELitTrue _) = return $ ImmediateBool 1
genExpr (ELitFalse _) = return $ ImmediateBool 0
genExpr (EApp _ (Ident ident) exprs) = do
  args <- mapM genExpr exprs
  st <- get
  let Just (Types.Fun t ts) = M.lookup ident (functionTypes st)
  let types = map show ts
  let argsString = createArgString types args
  case t of
    Types.Void -> do
      emit (Nothing, ICall "void" ident (zip types args))
      return $ ImmediateInt 1
    _ -> do
      temp <- genAddr t
      emit (Just temp, ICall (show t) ident (zip types args))
      return temp
genExpr (EString _ str) = do
  id <- saveStringLiteral str
  addr <- genAddr Types.Str
  emit (Just addr, IBitCast (length str + 1) id)
  return addr
genExpr (Not _ e) = do
  cond <- genExpr e
  trueLabel <- gets currentBlock
  falseLabel <- addBlock [trueLabel]
  finLabel <- addBlock [trueLabel]
  emit $ branch cond finLabel falseLabel

  setBlock falseLabel
  emit $ placeLabel falseLabel
  emit $ goto finLabel

  setBlock finLabel
  emit $ placeLabel finLabel
  tmp <- genAddr Types.Bool
  emit (Just tmp, IPhi "i1" [(ImmediateBool 0, trueLabel), (ImmediateBool 1, falseLabel)])
  return tmp
genExpr (Neg _ e) = genBinaryOp "sub i32" (ELitInt (Just (0, 0)) 0) e
genExpr (EAdd _ e op e') = genBinaryOp (addOpToLLVM op) e e'
genExpr (EMul _ e op e') = genBinaryOp (mulOpToLLVM op) e e'
genExpr (ERel _ e op e') = genCmp (relOpToLLVM op) e e'
genExpr (EAnd _ e e') = do
  l <- gets currentBlock
  midLabel <- addBlock [l]
  falseLabel <- addBlock [l, midLabel]
  finLabel <- addBlock [midLabel, falseLabel]
  cond <- genExpr e
  emit $ branch cond midLabel falseLabel

  setBlock midLabel
  emit $ placeLabel midLabel
  cond' <- genExpr e'
  lastMidLabel <- gets currentBlock
  emit $ branch cond' finLabel falseLabel

  setBlock falseLabel
  emit $ placeLabel falseLabel
  emit $ goto finLabel

  setBlock finLabel
  emit $ placeLabel finLabel
  tmp <- genAddr Types.Bool
  emit (Just tmp, IPhi "i1" [(ImmediateBool 1, lastMidLabel), (ImmediateBool 0, falseLabel)])
  return tmp
genExpr (EOr a e e') = do
  l <- gets currentBlock
  midLabel <- addBlock [l]
  falseLabel <- addBlock [midLabel]
  finLabel <- addBlock [l, falseLabel, midLabel]
  cond <- genExpr e
  entryLabel <- gets currentBlock
  emit $ branch cond finLabel midLabel

  setBlock midLabel
  emit $ placeLabel midLabel
  cond' <- genExpr e'
  lastMidLabel <- gets currentBlock
  emit $ branch cond' finLabel falseLabel

  setBlock falseLabel
  emit $ placeLabel falseLabel
  emit $ goto finLabel

  setBlock finLabel 
  emit $ placeLabel finLabel
  tmp <- genAddr Types.Bool
  emit (Just tmp, IPhi "i1" [(ImmediateBool 1, entryLabel), (ImmediateBool 1, lastMidLabel), (ImmediateBool 0, falseLabel)])
  return tmp

addOpToLLVM :: AddOp -> String
addOpToLLVM (Plus _) = "add i32"
addOpToLLVM (Minus _) = "sub i32"

relOpToLLVM :: RelOp -> String
relOpToLLVM (LTH _) = "slt"
relOpToLLVM (LE _) = "sle"
relOpToLLVM (GTH _) = "sgt"
relOpToLLVM (GE _) = "sge"
relOpToLLVM (EQU _) = "eq"
relOpToLLVM (NE _) = "ne"

mulOpToLLVM :: MulOp -> String
mulOpToLLVM (Times _) = "mul i32"
mulOpToLLVM (Div _) = "sdiv i32"
mulOpToLLVM (Mod _) = "srem i32"

genCmp :: String -> Expr -> Expr -> GenM Address
genCmp comp e e' = do
  addr <- genExpr e
  addr' <- genExpr e'
  c <- genAddr Types.Bool
  emit $ icmp comp c addr addr'
  return c

genBinaryOp :: String -> Expr -> Expr -> GenM Address
genBinaryOp op e e' = do
  addr <- genExpr e
  addr' <- genExpr e'
  emitBinaryOp op addr addr'

emitBinaryOp :: String -> Address -> Address -> GenM Address
emitBinaryOp op a a' = do
  let t = addrToLLVMType a
  let t' = addrToLLVMType a'
  if t == "i8*"
    then do
      addr <- genAddr Types.Str
      emit (Just addr, ICall "i8*" "__concat" [("i8*", a), ("i8*", a')])
      return addr
    else do
      addr <- genAddr Types.Int
      emit (Just addr, IBinOp op a a')
      return addr
