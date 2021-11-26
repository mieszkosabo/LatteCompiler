module Src.CodeGen.Utils where

import Control.Monad.Reader
import Data.List (intercalate, isPrefixOf)
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