{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Src.CodeGen.Utils where

import Control.Monad.Reader
import Control.Monad.State
import Data.List (intercalate, isPrefixOf)
import qualified Data.Map as M
import Parser.AbsLatte
import Src.CodeGen.State
import Src.CodeGen.State (GenState (GenState))
import Src.Frontend.Types (stripPositionFromType)

argsTypes :: [Arg] -> [String]
argsTypes [] = []
argsTypes ((Arg _ t _) : as) = show (stripPositionFromType t) : argsTypes as

addArgsToEnv :: [Arg] -> GenM ([Address], Env)
addArgsToEnv args = do
  env <- ask
  foldM f ([], env) $ reverse args
  where
    f = \(addresses, env) (Arg _ t (Ident ident)) -> local (const env) $ fun t addresses ident -- (b -> a -> m b)
    fun = \ty addresses ident -> do
      addr <- genAddr $ stripPositionFromType ty
      (addr, env') <- declareVar ident addr
      return (addr : addresses, env')

createArgString :: [String] -> [Address] -> String
createArgString types addresses = intercalate ", " $ zipWith (\t a -> t ++ " " ++ show a) types addresses

isImplicitReturn :: GenState -> Bool
isImplicitReturn = not . isExplicitReturn

isExplicitReturn :: GenState -> Bool
isExplicitReturn st = "\tret" `isPrefixOf` head (revCode st)

placeLabel :: Label -> Instr
placeLabel l = concat ["\tL", show l, ":"]

useLabel :: Label -> String
useLabel l = "label %L" ++ show l

goto :: Label -> Instr
goto l = unwords ["\tbr", useLabel l]

branch :: Address -> Label -> Label -> Instr
branch a t e = unwords ["\tbr i1", show a, ",", useLabel t, ",", useLabel e]

icmp :: String -> Address -> Address -> Address -> Instr
icmp comp c a a' = concat ["\t", show c, " = icmp ", comp, " i32 ", show a, ",", show a']

createPhiNodes :: Label -> [(Label, Store)] -> GenM ()
createPhiNodes currLabel pairs = do
  env <- ask
  mapM_ f $ M.toList env
  where
    f (varname, loc) = do
      s <- gets store
      let addr = s M.! loc
      let tmp = WithLabel varname currLabel addr
      let phiRhs = createPhiNode loc pairs
      emit $ concat ["\t", show tmp, " = phi " ++ addrToLLVMType addr, intercalate "," phiRhs]
      setVar varname tmp

createPhiNode :: Loc -> [(Label, Store)] -> [String]
createPhiNode _ [] = []
createPhiNode loc ((l, s) : rest) = concat ["[ ", show addr, ", %L", show l, "]"] : createPhiNode loc rest
  where
    addr = s M.! loc

wrapAllAddressesWithLabel :: Label -> GenM ()
wrapAllAddressesWithLabel l = do
  env <- ask
  forM_
    (M.keys env)
    ( \varname -> do
        a <- getVar varname
        setVar varname (WithLabel varname l a)
    )