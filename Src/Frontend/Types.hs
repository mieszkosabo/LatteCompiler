module Src.Frontend.Types where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Data.Map (Map)
import qualified Parser.AbsLatte as Abs

type VarName = String

type Pos = Abs.BNFC'Position

-- ReturnedType type is useful for determining the final type of some statements
-- that have if clauses with returns inside
data ReturnedType' a = ConditionalReturn a | Return a deriving (Show, Eq)

type ReturnedType = ReturnedType' (Maybe LatteType)

conditionalReturn :: ReturnedType -> ReturnedType
conditionalReturn (Return t) = ConditionalReturn t
conditionalReturn t = t

normalReturn :: ReturnedType -> ReturnedType
normalReturn = id

get :: ReturnedType -> Maybe LatteType
get (Return t) = t
get (ConditionalReturn t) = t

isConditionalReturn :: ReturnedType -> Bool
isConditionalReturn (ConditionalReturn _) = True
isConditionalReturn _ = False

data TypeCheckErrors
  = UseOfUndeclaredName Pos String
  | TypeAssertFailed Pos String String
  | ReturnTypeVary Pos String String
  | FunctionApplicationError Pos String
  | NoMainFunctionError
  | NameAlreadyExistsInScopeError Pos String
  | MainFunctionTakesArgumentsError Pos
  | MainFunctionMustReturnInt Pos
  | DifferentReturnTypes Pos
  | FunctionArgumentModification Pos
  | OtherError Pos String

addPositionInfo :: Pos -> String
addPositionInfo (Just (line, col)) = " near line " ++ show line ++ ", column " ++ show col ++ "."
addPositionInfo Nothing = "."

instance Show TypeCheckErrors where
  show (UseOfUndeclaredName pos name) = "Undeclared name: " ++ name ++ addPositionInfo pos
  show (TypeAssertFailed pos t1 t2) = "Expected type: " ++ t1 ++ ", but got: " ++ t2 ++ addPositionInfo pos
  show (ReturnTypeVary pos t1 t2) = "Unexpected return type. Expected: " ++ t1 ++ ", but got: " ++ t2 ++ addPositionInfo pos
  show (DifferentReturnTypes pos) = "Function returns different types" ++ addPositionInfo pos
  show (FunctionApplicationError pos msg) = "Error in function application: " ++ msg ++ addPositionInfo pos
  show NoMainFunctionError = "Could not find `main` function."
  show (NameAlreadyExistsInScopeError pos name) = "Name " ++ name ++ " is already occupied by other variable in scope" ++ addPositionInfo pos
  show (MainFunctionTakesArgumentsError pos) = "Main function can't take arguments" ++ addPositionInfo pos
  show (MainFunctionMustReturnInt pos) = "Main function must return int" ++ addPositionInfo pos
  show (OtherError pos msg) = msg ++ addPositionInfo pos
  show (FunctionArgumentModification pos) = "Attempted to modify function argument " ++ addPositionInfo pos

-- atm isMutable flag isn't much utilized, but might become handy when I add `const` modifier
type TEnv = Map VarName (LatteType, Bool) -- (type, isMutable)

type LocalScope = [VarName]

type StaticCheck = ReaderT TEnv (ExceptT TypeCheckErrors IO)

data LatteType = Int | Str | Bool | Void | Fun LatteType [LatteType] deriving (Show, Eq)

-- build in functions types
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
