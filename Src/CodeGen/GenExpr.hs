{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Src.CodeGen.GenExpr where

import Control.Monad.State
import Data.List (intercalate, elemIndex, find)
import qualified Data.Map as M
import Parser.AbsLatte
import Src.CodeGen.State
import Src.CodeGen.Utils
import qualified Src.Frontend.Types as Types
import Src.Frontend.Types (stripPositionFromType)
import Data.Maybe (fromMaybe)

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
  emit (Just addr, IBitCast ("[" ++ show (length str + 1) ++ " x i8]*") id "i8*")
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
genExpr (ENewArr _ t e) = do
  len <- genExpr e
  let ty = stripPositionFromType t
  structAddrNotCasted <- genArrAddr ty
  structAddr <- genArrAddr ty
  size <- getLLVMTypeSize "%Arr" (ImmediateInt 1)
  emit (Just structAddrNotCasted, ICall "i8*" "malloc" [("i32", size)])
  emit (Just structAddr, IBitCast "i8*" (show structAddrNotCasted) "%Arr*")
  innerArrId <- freshId
  let arrAddr = ArrAddr ty innerArrId
  arrTmp <- genTempPointerAddr
  lenTimesSizeOfInt <- getLLVMTypeSize "i32" len
  emit (Just arrTmp, ICall "i8*" "malloc" [("i32", lenTimesSizeOfInt)])
  emit (Just arrAddr, IBitCast "i8*" (show arrTmp) (show ty ++ "*")) -- create inner arr
  
  -- set length
  lengthAddr <- genTempPointerAddr 
  emit (Just lengthAddr, IGEP "%Arr" (show structAddr) [ImmediateInt 0, ImmediateInt 1])
  emit (Nothing, IStore "i32" len lengthAddr)

  -- set inner Arr
  innerArrFieldAddr <- genTempPointerAddr
  emit (Just innerArrFieldAddr, IGEP "%Arr" (show structAddr) [ImmediateInt 0, ImmediateInt 0])
  castedAddr <- genTempPointerAddr
  emit (Just castedAddr, IBitCast (show ty ++ "*") (show arrAddr) "i32*")
  emit (Nothing, IStore "i32*" castedAddr innerArrFieldAddr)

  return structAddr

genExpr arrGet@(EArrGet _ e e') = do
  (valAddr, ty) <- getElementPointer arrGet
  val <- genAddr ty
  emit (Just val, ILoad (show ty) valAddr)
  return val

genExpr (EProp _ e (Ident ident)) = do
  pointerAddr <- genExpr e
  let unWrapped = unWrapAddr pointerAddr
  case unWrapped of 
    (ArrAddr t id) -> do
      lenPointer <- genTempPointerAddr
      emit (Just lenPointer, IGEP "%Arr" (show pointerAddr) [ImmediateInt 0, ImmediateInt 1])
      len <- genAddr Types.Int
      emit (Just len, ILoad "i32" lenPointer)
      return len 
    (PointerAddr id name) -> do
      clses <- gets classes
      let cls = clses M.! name
      attrPointer <- genTempPointerAddr
      let (attrIdx, ty) = getAttrIdxAndType cls ident
      emit (Just attrPointer, IGEP ("%" ++ name) (show pointerAddr) [ImmediateInt 0, ImmediateInt attrIdx])
      val <- genAddr ty 
      emit (Just val, ILoad (show ty) attrPointer)
      return val
    r -> liftIO (print r) >> return undefined
genExpr (ENewClass _ (ClassType _ (Ident clsName))) = do
  ptrAddr <- genPointerAddr clsName
  ptrAddrNotCasted <- genTempPointerAddr
  size <- getLLVMTypeSize ("%" ++ clsName) (ImmediateInt 1)
  emit (Just ptrAddrNotCasted, ICall "i8*" "malloc" [("i32", size)])
  emit (Just ptrAddr, IBitCast "i8*" (show ptrAddrNotCasted) ("%" ++ clsName ++ "*"))
  emit (Nothing, ICall "void" (clsName ++ "_constructor") [("%" ++ clsName ++ "*", ptrAddr)])
  return ptrAddr
genExpr (ENullCast _ (Ident clsName)) = return $ NullPtr ("%" ++ clsName ++ "*")
genExpr (EPropApp _ e (Ident ident) exprs) = do
  pointerAddr <- genExpr e
  args <- mapM genExpr exprs
  clses <- gets classes
  let (PointerAddr _ name) = unWrapAddr pointerAddr
  let cls = clses M.! name

  vtablePtr <- genTempPointerAddr
  emit (Just vtablePtr, IGEP ("%" ++ name) (show pointerAddr) [ImmediateInt 0, ImmediateInt 0])
  vtablePtr' <- genTempPointerAddr
  emit (Just vtablePtr', ILoad ("%" ++ name ++ "_vtable_type*") vtablePtr)
  vtable <- genTempPointerAddr
  let Just (methodT, idx) = findWithIndex (\(c, n, tt) -> n == ident) (methods cls)
  let (ogname, _,  Types.Fun t types) = methodT
  emit (Just vtable, IGEP ("%" ++ name ++ "_vtable_type") (show vtablePtr') [ImmediateInt 0, ImmediateInt idx])
  funPtr <- genTempPointerAddr
  emit (Just funPtr, ILoad (createFunctionPointerType name methodT) vtable)

  newPointerAddr <- genPointerAddr ogname
  when (name /= ogname) (
    do
      emit (Just newPointerAddr, IBitCast ("%" ++ name ++ "*") (show pointerAddr) ("%" ++ ogname ++ "*"))
    )
  let pp = if name /= ogname then newPointerAddr else pointerAddr
  case t of
    Types.Void -> do
      emit (Nothing, ICall (show t) (ogname ++ "_" ++ ident) (("%" ++ ogname ++ "*", pp) : zip (map show types) args))
      return $ ImmediateInt 1
    _ -> do
      temp <- genAddr t
      emit (Just temp, ICall (show t) (ogname ++ "_" ++ ident) (("%" ++ ogname ++ "*", pp) : zip (map show types) args))
      return temp

createFunctionPointerType :: String -> (String, String, Types.LatteType) -> String
createFunctionPointerType clsName (ogClsName, _, Types.Fun retTy argTypes)
    = show retTy ++ "(" ++ intercalate ", " (("%" ++ ogClsName ++ "*") : map show argTypes) ++ ")*"

findWithIndex :: (a -> Bool) -> [a] -> Maybe (a, Int)
findWithIndex p as = find (p . fst) (zip as [0..])

-- gives pointer to element at idx in array or at propName in an object
getElementPointer :: Expr -> GenM (Address, Types.LatteType)
getElementPointer (EArrGet _ e e') = do
  structPointer <- genExpr e
  let unWrappedPtr@(ArrAddr ty _) = unWrapAddr structPointer
  idx <- genExpr e'
  innerArrPointer <- genTempPointerAddr
  emit (Just innerArrPointer, IGEP "%Arr" (show structPointer) [ImmediateInt 0, ImmediateInt 0])
  innerArr <- genTempPointerAddr
  emit (Just innerArr, ILoad "i32*" innerArrPointer)
  casted <- genTempPointerAddr
  emit (Just casted, IBitCast "i32*" (show innerArr) (show ty ++ "*"))
  valAddr <- genTempPointerAddr
  emit (Just valAddr, IGEP (show ty) (show casted) [idx])
  return (valAddr, ty)
getElementPointer (EProp _ e (Ident ident)) = do
  pointerAddr@(PointerAddr id name) <- genExpr e
  clses <- gets classes
  let cls = clses M.! name
  attrPointer <- genTempPointerAddr
  let (attrIdx, ty) = getAttrIdxAndType cls ident
  emit (Just attrPointer, IGEP ("%" ++ name) (show pointerAddr) [ImmediateInt 0, ImmediateInt attrIdx])
  return (attrPointer, ty) 
getElementPointer _ = undefined

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
emitBinaryOp op (ImmediateInt n) (ImmediateInt n') = case op of -- constant folding
  "add i32" -> return $ ImmediateInt (n + n')
  "sub i32" -> return $ ImmediateInt (n - n')
  "mul i32" -> return $ ImmediateInt (n * n')
  "sdiv i32" -> return $ ImmediateInt (n `div` n')
  "srem i32" -> return $ ImmediateInt (n `mod` n')
  _ -> undefined
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
