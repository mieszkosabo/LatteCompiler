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
    f = \(addresses, env) (Arg _ t (Ident ident)) -> local (const env) $ fun addresses ident -- (b -> a -> m b)
    fun = \addresses ident -> do
      id <- freshId
      (addr, env') <- declareVar ident (Right $ Local id)
      return (addr : addresses, env')

createArgString :: [String] -> [Address] -> String
createArgString types addresses = intercalate ", " $ zipWith (\t a -> t ++ " " ++ show a) types addresses

isImplicitReturn :: GenState -> Bool
isImplicitReturn st = not $ "\tret" `isPrefixOf` head (revCode st)

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
      let tmp = WithLabel varname currLabel (s M.! loc)
      let phiRhs = createPhiNode loc pairs
      emit $ concat ["\t", show tmp, " = phi i32 ", intercalate "," phiRhs]
      setVar varname tmp

createPhiNode :: Loc -> [(Label, Store)] -> [String]
createPhiNode _ [] = []
createPhiNode loc ((l, s) : rest) = concat ["[ ", show addr, ", %L", show l, "]"] : createPhiNode loc rest
  where
    addr = s M.! loc