module Src.CodeGen.State where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import qualified Src.Frontend.Types as Types

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
  | WithLabel VarName Label Address

instance Show Address where
  show (IntAddr i) = "%i" ++ show i
  show (ImmediateInt i) = show i
  show (BoolAddr i) = "%b" ++ show i
  show (ImmediateBool b) = show b
  show (StrAddr i) = "%s" ++ show i
  show (WithLabel vn l a) = if head str /= '%' then "%wl" ++ str else str
    where
      str = show a ++ "." ++ vn ++ "." ++ show l

type Env = M.Map VarName Loc

type Store = M.Map Loc Address

data GenState = GenState
  { nextId :: Int,
    store :: Store,
    revCode :: Code,
    functionTypes :: M.Map VarName Types.LatteType,
    stringLiterals :: [StringLiteral],
    lastLabel :: Label
  }
  deriving (Show)

restore :: Store -> GenM ()
restore st = modify (\s -> s {store = st})

setLastLabel :: Label -> GenM ()
setLastLabel l = modify (\s -> s {lastLabel = l})

data StringLiteral = StringLiteral
  { text :: String,
    stringId :: String
  }

instance Show StringLiteral where
  show sl = concat [stringId sl, " = private constant [", show $ length (text sl) + 1, " x i8] c", "\"", text sl, "\\00", "\""]

saveStringLiteral :: String -> GenM String
saveStringLiteral str = do
  id <- fmap (\i -> "@s" ++ show i) freshId
  let sl = StringLiteral {text = str, stringId = id}
  modify (\s -> s {stringLiterals = sl : stringLiterals s})
  return id

addStringLiteralsDefinitions :: [StringLiteral] -> GenM ()
addStringLiteralsDefinitions = mapM_ (emit . show)

freshId :: GenM Int
freshId = do
  s <- get
  let id = nextId s
  put s {nextId = id + 1}
  return id

freshLabel :: GenM Label
freshLabel = freshId

emit :: Instr -> GenM ()
emit instr = modify (\s -> s {revCode = instr : revCode s})

genAddr :: Types.LatteType -> GenM Address
genAddr Types.Bool = fmap BoolAddr freshId
genAddr Types.Int = fmap IntAddr freshId
genAddr Types.Str = fmap StrAddr freshId
genAddr _ = undefined

addrToLLVMType :: Address -> String
addrToLLVMType (IntAddr _) = "i32"
addrToLLVMType (ImmediateInt _) = "i32"
addrToLLVMType (BoolAddr _) = "i1"
addrToLLVMType (ImmediateBool _) = "i1"
addrToLLVMType (StrAddr _) = "i8*"
addrToLLVMType (WithLabel vn l a) = addrToLLVMType a

getVar :: VarName -> GenM Address
getVar ident = do
  env <- ask
  st <- get
  let loc = env M.! ident
  let addr = store st M.! loc
  return addr

setVar :: VarName -> Address -> GenM ()
setVar ident addr = do
  env <- ask
  st <- get
  let (Just loc) = M.lookup ident env
  modify (\s -> s {store = M.insert loc addr (store s)})

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
      lastLabel = 1
    }

runGen :: GenM a -> IO (a, GenState)
runGen m = runStateT (runReaderT m M.empty) initialState