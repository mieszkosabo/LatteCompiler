module Src.CodeGen.State where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import qualified Src.Frontend.Types as Types
import Data.List (intercalate, find, intercalate, elemIndex)
import Src.Frontend.Types (LatteType)
import Parser.AbsLatte (ClassStmt)
import Data.Maybe (fromMaybe)

getAttrIdxAndType :: LatteClass -> String -> (Int, Types.LatteType)
getAttrIdxAndType cls ident = (idx, ty)
  where 
    trueIdx = fromMaybe undefined (elemIndex ident (map fst (attributes cls)))
    idx = trueIdx + 1
    ty = snd $ attributes cls !! trueIdx

type Methods = [(String, String, LatteType)]
type Attributes = [(String, LatteType)]

data LatteClass = LatteClass {
    name       :: String,
    attributes :: Attributes,
    methods    :: Methods,
    subClasses :: [LatteClass],
    clsStmts   :: [ClassStmt]
} deriving (Show, Eq)

type VarName = String

type Instr = String

type Code = [Instr]

type Loc = Int

type Label = Int

data Address
  = IntAddr Int
  | ImmediateInt Int
  | BoolAddr Int
  | ImmediateBool Int
  | StrAddr Int
  | ArrAddr LatteType Int -- type id 
  | PointerAddr Int String
  | NullPtr String
  | WithLabel VarName Label Address
  deriving Eq

instance Show Address where
  show (IntAddr i) = "%i" ++ show i
  show (ImmediateInt i) = show i
  show (BoolAddr i) = "%b" ++ show i
  show (ImmediateBool b) = show b
  show (StrAddr i) = "%s" ++ show i
  show (ArrAddr _ i) = "%a" ++ show i
  show (PointerAddr i _) = "%p" ++ show i
  show (NullPtr ty) = "null"
  show (WithLabel vn l a) = if head str /= '%' then "%wl" ++ str else str
    where
      str = show a ++ "." ++ vn ++ "." ++ show l

unWrapAddr :: Address -> Address
unWrapAddr (WithLabel _ _ a) = unWrapAddr a
unWrapAddr a = a

isPointerAddr :: Address -> Bool
isPointerAddr (PointerAddr _ _) = True
isPointerAddr (ArrAddr _ _) = True
isPointerAddr _ = False

getPointerAddrId :: Address -> Int
getPointerAddrId (PointerAddr i _) = i
getPointerAddrId (ArrAddr t i) = i
getPointerAddrId (WithLabel _ _ a) = getPointerAddrId a
getPointerAddrId _ = undefined

type Env = M.Map VarName Loc

type Store = M.Map Loc Address

data GenState = GenState
  { nextId :: Int,
    store :: Store,
    revCode :: Code,
    blocks :: M.Map Label Block,
    currentBlock :: Label,
    functionTypes :: M.Map VarName Types.LatteType,
    stringLiterals :: [StringLiteral],
    classes :: M.Map String LatteClass,
    classesForest :: [LatteClass],
    currentClass :: Maybe LatteClass
  }
  deriving (Show)

-- Blocks are the vertices of the Control Flow Graph
data Block = LLVMBlock {
    label :: Label,
    instrs :: [IntermediateInstr],
    preds :: [Label],
    succs :: [Label]
} deriving Eq

instance Show Block where
  show b = unlines (map printIntermediateInstr (reverse $ instrs b))

clearBlocks :: GenM ()
clearBlocks = modify $ \s -> s { blocks = M.empty }

addBlock :: [Label] -> GenM Label
addBlock preds = do
  id <- freshLabel
  let newBlock = LLVMBlock { label = id, instrs = [], preds = preds, succs = [] }
  modify $ \s -> s { blocks = M.insert id newBlock (blocks s)}
  return id

setBlock :: Label -> GenM Label
setBlock label = do
  modify $ \s -> s { currentBlock = label}
  return label

getBlock :: GenM Block
getBlock = do
  b <- gets currentBlock
  blks <- gets blocks
  return $ blks M.! b

modifyBlock :: Block -> GenM ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = M.insert active new (blocks s)}

addSuccsToCurrentBlock :: [Label] -> GenM ()
addSuccsToCurrentBlock newSuccs = do
  active <- getBlock
  let new = active { succs = succs active ++ newSuccs }
  modifyBlock new

-- IR that is 1:1 LLVM, but easier to optimize than plain text
-- examples: 
-- %t1 = add 4, 3 ----> (just %t1, IBinOp "add i32" (ImmediateInt 4) (ImmediateInt 3))
-- ret 42         ----> (Nothing, IRet (ImmediateInt 42))
type IntermediateInstr = (Maybe Address, LLVMInstr) 

printIntermediateInstr :: IntermediateInstr -> String
printIntermediateInstr (Nothing, instr) = "\t" ++ show instr
printIntermediateInstr (Just addr, instr) = "\t" ++ show addr ++ " = " ++ show instr

type LLVMType = String

data LLVMInstr = 
    ICall LLVMType String [(String, Address)] -- type funName [(type, arg)]
    | IGoto Label
    | IBranch Address Label Label
    | ICmp String Address Address -- cmpOperator address1 address2
    | IPhi LLVMType [(Address, Label)] 
    | IBinOp String Address Address
    | IRet LLVMType Address
    | IVRet
    | IStringLiteralDef StringLiteral
    | ILabel Label
    | IGEPNull String
    | IGEP String String [Address]
    | IPtrToInt String Address
    | ILoad String Address
    | IStore String Address Address
    | IBitCast String String String
    | IMul Address Address
    deriving Eq

isCommonSubExpression :: LLVMInstr -> LLVMInstr -> Bool
-- isCommonSubExpression (IBitCast _ id) (IBitCast _ id') = id == id'
isCommonSubExpression (ICmp s a a') (ICmp s' a1 a2) = s == s' && a == a1 && a' == a2
isCommonSubExpression (IBinOp "mul i32" a a') (IBinOp "mul i32" a1 a2) = (a == a1 && a' == a2) || (a == a2 && a' == a1)
isCommonSubExpression (IBinOp "add i32" a a') (IBinOp "add i32" a1 a2) = (a == a1 && a' == a2) || (a == a2 && a' == a1)
isCommonSubExpression (IBinOp s a a') (IBinOp s' a1 a2) = s == s' && a == a1 && a' == a2
isCommonSubExpression _ _ = False

instance Show LLVMInstr where
    show (IMul addr addr') = concat ["mul i32", show addr, ", ", show addr']
    show (ICall ty name args) = concat ["call ", ty, name, "(", argsString, ")"]
        where argsString = intercalate ", " $ map (\(argTy, arg) -> argTy ++ " " ++ show arg) args
    show (IBitCast ty1 val ty2 ) = concat ["bitcast ", ty1, " ", val, " to ", ty2]
    show (IGoto label) = "br label %L" ++ show label
    show (IBranch addr thenLabel elseLabel) = concat ["br i1 ", show addr, ", label %L",  show thenLabel, ", label %L", show elseLabel]
    show (ICmp op addr addr') = concat ["icmp ", op, " ", addrToLLVMType addr, " ", show addr, ", ", show addr']
    show (IPhi ty rhs) = "phi " ++  ty
      ++
      intercalate
      ","
      (map
         (\ (addr, label) -> concat ["[ ", show addr, ", %L", show label, "]"])
         rhs)
    show (IBinOp op addr addr') = concat [op, " ", show addr, ", ", show addr']
    show (IRet ty addr) = concat ["ret ", ty, " ", show addr]
    show IVRet = "ret void"
    show (IStringLiteralDef sl) = show sl ++ "\n"
    show (ILabel l) = concat ["L", show l, ":"]
    show (IGEPNull s) = concat ["getelementptr ", s, ", ", s, "* null, i32 1"]
    show (IGEP ty addr idxs) = concat ["getelementptr ", ty, ", ", ty, "* ", addr, ", ", intercalate "," (map (\x -> "i32 " ++ show x) idxs)]
    show (IPtrToInt s addr) = concat ["ptrtoint ", s, "* ", show addr, " to i32"]
    show (ILoad ty addr) = "load " ++ ty ++ ", " ++ ty ++ "* " ++ show addr
    show (IStore ty addr addr') = "store " ++ ty ++ " " ++ show addr ++ ", " ++ ty ++ "* " ++ show addr'



restore :: Store -> GenM ()
restore st = modify (\s -> s {store = st})

data StringLiteral = StringLiteral
  { text :: String,
    stringId :: String
  }
instance Eq StringLiteral where
  sl == sl' = stringId sl == stringId sl'

instance Show StringLiteral where
  show sl = concat [stringId sl, " = private constant [", show $ length (text sl) + 1, " x i8] c", "\"", text sl, "\\00", "\""]

saveStringLiteral :: String -> GenM String
saveStringLiteral str = do
  st <- get
  case find (\sl -> text sl == str) (stringLiterals st) of
    Just sl -> return $ stringId sl
    Nothing -> do
      id <- fmap (\i -> "@s" ++ show i) freshId
      let sl = StringLiteral {text = str, stringId = id}
      modify (\s -> s {stringLiterals = sl : stringLiterals s})
      return id

addStringLiteralsDefinitions :: [StringLiteral] -> String -> IO ()
addStringLiteralsDefinitions literals filename 
  = mapM_  (\sl -> appendFile filename $ printIntermediateInstr (Nothing, IStringLiteralDef sl)) literals

freshId :: GenM Int
freshId = do
  s <- get
  let id = nextId s
  put s {nextId = id + 1}
  return id

freshLabel :: GenM Label
freshLabel = freshId

emit :: IntermediateInstr -> GenM ()
emit instr@(_, IBranch _ l l') = do
  b <- getBlock
  let b' = b { instrs = instr : instrs b }
  modifyBlock b'
emit instr@(_, IGoto l) = do
  addSuccsToCurrentBlock [l]
  b <- getBlock
  let b' = b { instrs = instr : instrs b }
  modifyBlock b'
emit instr = do
  b <- getBlock
  let b' = b { instrs = instr : instrs b }
  modifyBlock b'

genAddr :: Types.LatteType -> GenM Address
genAddr Types.Bool = fmap BoolAddr freshId
genAddr Types.Int = fmap IntAddr freshId
genAddr Types.Str = fmap StrAddr freshId
genAddr (Types.Array t) = ArrAddr t <$> freshId
genAddr (Types.StrippedCls s) = genPointerAddr s
genAddr a = liftIO $ print a >> undefined

genTempPointerAddr :: GenM Address
genTempPointerAddr = genPointerAddr "temp"

genPointerAddr :: String -> GenM Address
genPointerAddr name = do
  id <- freshId
  return $ PointerAddr id name
genArrAddr :: Types.LatteType -> GenM Address
genArrAddr t = ArrAddr t <$> freshId

addrToLLVMType :: Address -> String
addrToLLVMType (IntAddr _) = "i32"
addrToLLVMType (ImmediateInt _) = "i32"
addrToLLVMType (BoolAddr _) = "i1"
addrToLLVMType (ImmediateBool _) = "i1"
addrToLLVMType (StrAddr _) = "i8*"
addrToLLVMType (ArrAddr t _) = "%Arr*"
addrToLLVMType (WithLabel vn l a) = addrToLLVMType a
addrToLLVMType (PointerAddr id name) = "%" ++ name ++ "*"
addrToLLVMType (NullPtr s) = s

getVar :: VarName -> GenM Address
getVar ident = do
  env <- ask
  st <- get
  let loc = M.lookup ident env
  case loc of
    Just l -> do
      let addr = store st M.! l
      return addr
    Nothing -> do
      (Just cls) <- gets currentClass
      attrPointer <- genTempPointerAddr
      let (attrIdx, ty) = getAttrIdxAndType cls ident
      thisAddr <- getVar "self"
      emit (Just attrPointer, IGEP ("%" ++ name cls) (show thisAddr) [ImmediateInt 0, ImmediateInt attrIdx])
      addr <- genAddr ty
      emit (Just addr, ILoad (show ty) attrPointer)
      return addr

setVar :: VarName -> Address -> GenM ()
setVar ident addr = do
  env <- ask
  st <- get
  let loc = M.lookup ident env
  case loc of
    Just l -> do
      stor <- gets store 
      let oldAddr = stor M.! l
      modify (\s -> s {store = M.insert l addr (store s)})
      when (isClassPointer oldAddr) (do
        let (PointerAddr _ n) = unWrapAddr oldAddr
        let (PointerAddr _ n') = unWrapAddr addr
        when (n /= n') (do
          casted <- genPointerAddr n
          emit (Just casted, IBitCast ("%" ++ n' ++ "*") (show addr) ("%" ++ n ++ "*"))
          modify (\s -> s {store = M.insert l casted (store s)})
          )
        )
    Nothing -> do
      (Just cls) <- gets currentClass
      attrPointer <- genTempPointerAddr
      let (attrIdx, ty) = getAttrIdxAndType cls ident
      thisAddr <- getVar "self"
      emit (Just attrPointer, IGEP ("%" ++ name cls) (show thisAddr) [ImmediateInt 0, ImmediateInt attrIdx])
      emit (Nothing, IStore (show ty) addr attrPointer)

isClassPointer PointerAddr {} = True
isClassPointer _  = False

declareVar :: VarName -> Address -> GenM (Address, Env)
declareVar ident addr = do
  env <- ask
  st <- get
  loc <- freshId
  let env' = M.insert ident loc env
  modify (\s -> s {store = M.insert loc addr (store s)})
  return (addr, env')

type GenM a = ReaderT Env (StateT GenState IO) a

initialState =
  GenState
    { nextId = 1,
      store = M.empty,
      revCode = [],
      functionTypes = M.empty,
      stringLiterals = [],
      blocks = M.empty,
      currentBlock = -1,
      classes = M.empty,
      classesForest = [],
      currentClass = Nothing
    }

runGen :: GenM a -> IO (a, GenState)
runGen m = runStateT (runReaderT m M.empty) initialState