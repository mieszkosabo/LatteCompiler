module Src.CodeGen.GenExpr where

import Control.Monad.State
import Parser.AbsLatte
import Src.CodeGen.State
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Src.Frontend.Types as T
import Src.CodeGen.Utils (createArgString, icmp)

genExpr :: Expr -> GenM Address
genExpr (ELitInt _ n) = return $ Literal $ fromInteger n
genExpr (EVar _ (Ident ident)) = getVar ident
genExpr (ELitTrue _) = return $ Literal 1
genExpr (ELitFalse _) = return $ Literal 0
genExpr (EApp _ (Ident ident) exprs) = do
    temp <- genTemp
    args <- mapM genExpr exprs
    st <- get
    let Just (T.Fun t ts) = M.lookup ident (functionTypes st)
    let types = map show ts
    let argsString = createArgString types args
    case t of
        T.Void -> emit $ concat ["\tcall void @", ident, "(", argsString, ")" ] 
        _ -> emit $ concat ["\t", show temp, " = call ", show t ++ " @", ident, "(", argsString, ")"]
    return temp
genExpr (EString _ str) = do
    id <- saveStringLiteral str
    addr <- genStrAddr
    emit $ concat ["\t", show addr, " = bitcast [", show (length str + 1), " x i8]* ", id, " to i8*"]
    return addr
    -- | Not a (Expr' a)
genExpr (Neg _ e) = genBinaryOp "sub i32" (ELitInt (Just (0,0)) 0) e
genExpr (EAdd _ e op e') = genBinaryOp (addOpToLLVM op) e e'
genExpr (EMul _ e op e') = genBinaryOp (mulOpToLLVM op) e e'
genExpr (ERel _ e op e') = genCmp (relOpToLLVM op) e e'
    -- | EAnd a (Expr' a) (Expr' a)
    -- | EOr a (Expr' a) (Expr' a)

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
genCmp comp e e'= do
    addr <- genExpr e
    addr' <- genExpr e'
    temp <- genTemp
    emit $ icmp comp temp addr addr'
    return temp

genBinaryOp :: String -> Expr -> Expr -> GenM Address
genBinaryOp op e e' = do
    addr <- genExpr e
    addr' <- genExpr e'
    emitBinaryOp op addr addr'

emitBinaryOp :: String -> Address -> Address -> GenM Address
emitBinaryOp "add i32" a@(StrAddr i) a'@(StrAddr i') = do -- addition op overloaded for concatenating strings
    strAddr <- genStrAddr
    emit $ concat ["\t", show strAddr, " = call i8* @__concat(i8* ", show a, ", i8* ", show a', ")"]
    return strAddr
emitBinaryOp op a a' = do
    temp <- genTemp
    emit $ concat ["\t", show temp, " = ", op, " ", show a, ", ", show a']
    return temp