module Src.CodeGen.State where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import Src.Frontend.Types (LatteType)

type VarName = String

type Instr = String

type Code = [Instr]

type Loc = Int

type Label = Int

data Address
  = Literal Int
  | Local Int
  | Temp Int
  | StrAddr Int

instance Show Address where
  show (Literal i) = show i
  show (Local i) = "%l" ++ show i
  show (Temp i) = "%t" ++ show i
  show (StrAddr i) = "%s" ++ show i

type Env = M.Map VarName Loc

type Store = M.Map Loc Address

data GenState = GenState
  { nextId :: Int,
    store :: Store,
    revCode :: Code,
    functionTypes :: M.Map VarName LatteType,
    stringLiterals :: [StringLiteral]
  }
  deriving (Show)

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

genTemp :: GenM Address
genTemp = fmap Temp freshId

genStrAddr :: GenM Address
genStrAddr = fmap StrAddr freshId

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

declareVar :: VarName -> Either Int Address -> GenM (Address, Env)
declareVar ident initialValue = do
  env <- ask
  st <- get
  loc <- freshId
  let addr = case initialValue of
        Right addr -> addr
        Left n -> Literal n
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
      stringLiterals = []
    }

runGen :: GenM a -> IO (a, GenState)
runGen m = runStateT (runReaderT m M.empty) initialState