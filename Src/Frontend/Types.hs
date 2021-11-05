module Src.Frontend.Types where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Data.Map (Map)
import qualified Parser.AbsLatte as Abs

type VarName = String

type Pos = Abs.BNFC'Position

type ReturnedType = Maybe LatteType

data TypeCheckErrors
  = UseOfUndeclaredName Pos String
  | TypeAssertFailed Pos String String
  | ReturnTypeVary Pos String String
  | FunctionApplicationError Pos String
  | NoMainFunctionError
  | NameAlreadyExistsInScopeError Pos String
  | MainFunctionTakesArgumentsError Pos
  | MainFunctionMustReturnInt Pos
  | DifferentReturnTypes
  | OtherError Pos String

addPositionInfo :: Pos -> String
addPositionInfo (Just (line, col)) = " near line " ++ show line ++ ", column " ++ show col ++ "."
addPositionInfo Nothing = "."

instance Show TypeCheckErrors where
  show (UseOfUndeclaredName pos name) = "Undeclared name: " ++ name ++ addPositionInfo pos
  show (TypeAssertFailed pos t1 t2) = "Expected type: " ++ t1 ++ ", but got: " ++ t2 ++ addPositionInfo pos
  show (ReturnTypeVary pos t1 t2) = "Unexpected return type. Expected: " ++ t1 ++ ", but got: " ++ t2 ++ addPositionInfo pos
  show (DifferentReturnTypes) = "Function returns different types"
  show (FunctionApplicationError pos msg) = "Error in function application: " ++ msg ++ addPositionInfo pos
  show NoMainFunctionError = "Could not find `main` function."
  show (NameAlreadyExistsInScopeError pos name) = "Name " ++ name ++ " is already occupied by other variable in scope" ++ addPositionInfo pos
  show (MainFunctionTakesArgumentsError pos) = "Main function can't take arguments" ++ addPositionInfo pos
  show (MainFunctionMustReturnInt pos) = "Main function must return int" ++ addPositionInfo pos
  show (OtherError pos msg) = msg ++ addPositionInfo pos

type TEnv = Map VarName LatteType

type StaticCheck = ReaderT TEnv (ExceptT TypeCheckErrors IO)

data LatteType = Int | Str | Bool | Void | Fun LatteType [LatteType] deriving (Show, Eq)

printIntType :: LatteType
printIntType = Fun Void [Int]

printStringType :: LatteType
printStringType = Fun Void [Str]

errorType :: LatteType
errorType = Fun Void []

readIntType :: LatteType
readIntType = Fun Int []

readStringType :: LatteType
readStringType = Fun Str []

predefinedFunctionsTypes :: [(String, LatteType)]
predefinedFunctionsTypes =
  [ ("printInt", printIntType),
    ("printString", printStringType),
    ("error", errorType),
    ("readInt", readIntType),
    ("readString", readStringType)
  ]

stripPositionFromType :: Abs.Type -> LatteType
stripPositionFromType (Abs.Int _) = Int
stripPositionFromType (Abs.Str _) = Str
stripPositionFromType (Abs.Bool _) = Bool
stripPositionFromType (Abs.Void _) = Void
stripPositionFromType (Abs.Fun _ retType argTypes) =
  Fun (stripPositionFromType retType) (map stripPositionFromType argTypes)
