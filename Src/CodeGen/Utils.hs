{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Src.CodeGen.Utils where

import Control.Monad.Reader
import Control.Monad.State
import Data.List (intercalate, isPrefixOf)
import qualified Data.Map as M
import Parser.AbsLatte
import Src.CodeGen.State
import Src.Frontend.Types (stripPositionFromType, LatteType (Array, Int))

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
      let t = stripPositionFromType ty
      addr <- genAddr t
      (addr, env') <- declareVar ident addr
      return (addr : addresses, env')

createArgString :: [String] -> [Address] -> String
createArgString types addresses = intercalate ", " $ zipWith (\t a -> t ++ " " ++ show a) types addresses

isImplicitReturn :: GenState -> Bool
isImplicitReturn = not . isExplicitReturn

isExplicitReturn :: GenState -> Bool
isExplicitReturn st = case head (instrs b) of
  (_, IRet _ _) -> True
  (_, IVRet) -> True
  _ -> False
  where b = blocks st M.! currentBlock st

placeLabel :: Label -> IntermediateInstr 
placeLabel l = (Nothing, ILabel l) 

useLabel :: Label -> String
useLabel l = "label %L" ++ show l

goto :: Label -> IntermediateInstr 
goto l = (Nothing, IGoto l)

branch :: Address -> Label -> Label -> IntermediateInstr
branch a t e = (Nothing, IBranch a t e)

icmp :: String -> Address -> Address -> Address -> IntermediateInstr
icmp comp c a a' = (Just c, ICmp comp a a')

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
      unless 
        (areAllTheSame (map fst phiRhs) || isPointerAddr addr) -- don't create redundant phi nodes
        (
          do 
          emit (Just tmp, IPhi (addrToLLVMType addr) phiRhs)
          setVar varname tmp
        )

createPhiNode :: Loc -> [(Label, Store)] -> [(Address, Label)]
createPhiNode _ [] = []
createPhiNode loc ((l, s) : rest) =  (addr, l) : createPhiNode loc rest
  where
    addr = s M.! loc

wrapAllAddressesWithLabel :: Label -> GenM ()
wrapAllAddressesWithLabel l = do
  env <- ask
  forM_
    (M.keys env)
    ( \varname -> do
        a <- getVar varname
        unless (isPointerAddr a) (setVar varname (WithLabel varname l a))
    )

areAllTheSame :: Eq a => [a] -> Bool
areAllTheSame [] = True
areAllTheSame (x:xs) = areAllTheSame' x xs

areAllTheSame' :: Eq a => a -> [a] -> Bool
areAllTheSame' e = foldr (\ x -> (&&) (e == x)) True

getLLVMTypeSize :: String -> Address -> GenM Address
getLLVMTypeSize typeName multiplier = do
  addr <- genPointerAddr
  emit (Just addr, IGEPNull typeName)
  size <- genAddr Src.Frontend.Types.Int
  emit (Just size, IPtrToInt typeName addr)
  multipliedSize <- genAddr  Src.Frontend.Types.Int
  emit (Just multipliedSize, IMul size (ImmediateInt 32))
  return multipliedSize
