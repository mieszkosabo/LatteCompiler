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

getT :: ReturnedType -> Maybe LatteType
getT (Return t) = t
getT (ConditionalReturn t) = t

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
  | InvalidAssignment Pos
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
  show (InvalidAssignment pos) = "Invalid assignment" ++ addPositionInfo pos

-- atm isMutable flag isn't much utilized, but might become handy when I add `const` modifier
type TEnv = Map VarName (LatteType, Bool) -- (type, isMutable)

type LocalScope = [VarName]

type StaticCheck = ReaderT TEnv (ExceptT TypeCheckErrors IO)

data LatteType = 
  Int 
  | Str 
  | Bool 
  | Void 
  | Fun LatteType [LatteType] 
  | StrippedCls String
  | Cls String [(String, LatteType)] [(String, LatteType)] (Maybe String) -- name, methods (name, type), attributes (name, types), maybe superclass name
  | Array LatteType

instance Eq LatteType where
  Int == Int = True
  Str == Str = True
  Bool == Bool = True
  Void == Void = True
  (Fun t ts) == (Fun t' ts') = t == t' && ts == ts'
  (StrippedCls s) == (StrippedCls s') = s == s'
  (Cls s _ _ _) == (Cls s' _ _ _) = s == s'
  (StrippedCls s) == (Cls s' _ _ _) = s == s'
  (Cls s' _ _ _) == (StrippedCls s) = s == s'
  (Array t) == (Array t') = t == t'
  _ == _ = False


instance Show LatteType where
  show Int = "i32"
  show Str = "i8*"
  show Bool = "i1"
  show Void = "void"
  show (Fun t ts) = show t ++ " (" ++ concatMap show ts ++ ")"
  show (Array t) = "%Arr*"
  show (Cls n _ _ _) = "%" ++ n
  show (StrippedCls s) = "%" ++ s ++ "*"

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
    ("readString", readStringType),
    ("malloc", Fun Str [Int])
  ]

stripPositionFromType :: Abs.Type -> LatteType
stripPositionFromType (Abs.Int _) = Int
stripPositionFromType (Abs.Str _) = Str
stripPositionFromType (Abs.Bool _) = Bool
stripPositionFromType (Abs.Void _) = Void
stripPositionFromType (Abs.Fun _ retType argTypes) =
  Fun (stripPositionFromType retType) (map stripPositionFromType argTypes)
stripPositionFromType (Abs.List _ t) = Array $ stripPositionFromType t
stripPositionFromType (Abs.ClassType _ (Abs.Ident ident)) = StrippedCls ident