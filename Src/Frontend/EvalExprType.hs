module Src.Frontend.EvalExprType where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map as M
import Data.Maybe (isNothing)
import Parser.AbsLatte hiding (Bool, Fun, Int, Str)
import Src.Frontend.Types

evalExprType :: Expr -> StaticCheck LatteType
evalExprType (EVar pos (Ident ident)) = do
  env <- ask
  let maybeT = M.lookup ident env
  case maybeT of
    Nothing -> throwError $ UseOfUndeclaredName pos ident
    Just (t, isMutable) -> return t
evalExprType (ELitInt _ _) = return Int
evalExprType (ELitTrue _) = return Bool
evalExprType (ELitFalse _) = return Bool
evalExprType (EString _ _) = return Str
evalExprType (ENewArr pos t e) = do
  arrSizeType <- evalExprType e
  let arrType = stripPositionFromType t
  when (arrSizeType /= Int) (throwError $ OtherError pos "Array size must be an int")
  case arrType of
    (Array _) -> throwError $ OtherError pos "Arrays must be 1 dimensional"
    _ -> return $ Array arrType
evalExprType (EProp pos e (Ident propName)) = do
  leftDotSide <- evalExprType e
  case leftDotSide of
    (Array _) -> if propName == "length" then return Int else throwError $ OtherError pos "Arrays don't have properties other than `length`"
    -- TODO: objects
    _ -> throwError $ OtherError pos $ "Can't read property `" ++ propName ++ "` of " ++ show leftDotSide
evalExprType (EArrGet pos e e') = do
  arrT <- evalExprType e
  idxT <- evalExprType e'
  when (idxT /= Int) (throwError $ OtherError pos "Indices must be integers")
  case arrT of
    (Array t) -> return t
    _ -> throwError $ OtherError pos $ "Can't read index from " ++ show arrT
evalExprType (Not pos e) = do
  t <- evalExprType e
  if t /= Bool
    then throwError (TypeAssertFailed pos "bool" (show t))
    else return Bool
evalExprType (Neg pos e) = do
  t <- evalExprType e
  if t /= Int
    then throwError (TypeAssertFailed pos "int" (show t))
    else return Int
evalExprType (EMul pos e op e') = do
  checkIfBothExprsAreOfTheSameType [Int] e e' pos
  return Int
evalExprType (EAdd pos e op e') = checkIfBothExprsAreOfTheSameType [Int, Str] e e' pos
evalExprType (ERel pos e op e') = do
  case op of
    EQU ma -> checkIfBothExprsAreOfTheSameType [Int, Str, Bool] e e' pos
    NE ma -> checkIfBothExprsAreOfTheSameType [Int, Str, Bool] e e' pos
    _ -> checkIfBothExprsAreOfTheSameType [Int] e e' pos
  return Bool
evalExprType (EAnd pos e e') = do
  checkIfBothExprsAreOfTheSameType [Bool] e e' pos
  return Bool
evalExprType (EOr pos e e') = do
  checkIfBothExprsAreOfTheSameType [Bool] e e' pos
  return Bool
evalExprType (EApp pos (Ident ident) exprs) = do
  env <- ask
  let maybeFunT = M.lookup ident env
  when
    (isNothing maybeFunT)
    (throwError $ UseOfUndeclaredName pos ident)
  let Just (Fun retT argTypes, _) = maybeFunT

  -- reject if argument types don't match function type
  exprsTypes <- mapM evalExprType exprs
  when (argTypes /= exprsTypes) (throwError $ FunctionApplicationError pos "Invalid arguments type")
  return retT

checkIfBothExprsAreOfTheSameType :: [LatteType] -> Expr -> Expr -> Pos -> StaticCheck LatteType
checkIfBothExprsAreOfTheSameType acceptableTypes e e' pos = do
  t <- evalExprType e
  t' <- evalExprType e'
  when
    (t /= t')
    (throwError (OtherError pos "Operator requires that both operands are of the same type"))
  unless
    (t `elem` acceptableTypes)
    (throwError (OtherError pos $ "Invalid type" ++ show t ++ ", should be one of: " ++ show acceptableTypes))
  return t