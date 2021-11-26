module Src.CodeGen.GenExpr where

import Control.Monad.State
import Parser.AbsLatte
import Src.CodeGen.State
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Src.Frontend.Types as T


genExpr :: Expr -> GenM Address
genExpr (ELitInt _ n) = return $ Literal $ fromInteger n
genExpr (EVar _ (Ident ident)) = getVar ident
    -- | ELitTrue a
    -- | ELitFalse a
genExpr (EApp _ (Ident ident) exprs) = do
    temp <- genTemp
    args <- mapM genExpr exprs
    st <- get
    let Just (T.Fun t ts) = M.lookup ident (functionTypes st)
    let argsString = intercalate ", " (map (\a -> "i32 " ++ show a) args) -- FIXME: handle different types
    case t of
        T.Void -> emit $ concat ["\tcall void @", ident, "(", argsString, ")" ] 
        _ -> emit $ concat ["\t", show temp, " = call ", show t ++ " @", ident, "(", argsString, ")"]
    return temp
    -- | EString a String
    -- | Not a (Expr' a)
genExpr (Neg _ e) = undefined -- TODO:
genExpr (EAdd _ e op e') = genBinaryOp (addOpToLLVM op) e e'
genExpr (EMul _ e op e') = genBinaryOp (mulOpToLLVM op) e e'
    -- | ERel a (Expr' a) (RelOp' a) (Expr' a)
    -- | EAnd a (Expr' a) (Expr' a)
    -- | EOr a (Expr' a) (Expr' a)

addOpToLLVM :: AddOp -> String
addOpToLLVM (Plus _) = "add i32"
addOpToLLVM (Minus _) = "add i32"

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

genBinaryOp :: String -> Expr -> Expr -> GenM Address
genBinaryOp op e e' = do
    addr <- genExpr e
    addr' <- genExpr e'
    temp <- genTemp
    emit $ concat ["\t", show temp, " = ", op, " ", show addr, ", ", show addr']
    return temp