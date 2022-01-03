module Src.CodeGen.Optimization where
import Src.CodeGen.State
import Data.List ( (\\), nub, find )
import MonadUtils (MonadIO(liftIO))
import Control.Monad.List (forM_, foldM, when)
import Data.Bifunctor (second)

lcse :: Block -> GenM Block
lcse b = do
    newB <- foldM (\b' (lhs, rhs) -> do
        let repetitions = reverse $ filter (\(_, el) -> isCommonSubExpression  el rhs) (instrs b)
        if null repetitions 
            then return b' 
            else do
                let (Just repr) = fst $ head repetitions
                let addrsToRemove = map fst $ tail repetitions
                let withoutRepetitions = removeRepeatedInstrs b' addrsToRemove
                let withReplacements = map (second (replaceAddressesWithAddress addrsToRemove repr)) (instrs withoutRepetitions)
                return b' { instrs = withReplacements }
        )
        b 
        (instrs b)
    if b == newB 
        then return b 
        else lcse newB

removeRepeatedInstrs :: Block -> [Maybe Address] -> Block
removeRepeatedInstrs b repeated = b { instrs = newInstr}
    where newInstr = 
            foldr 
                (\instr rest -> if fst instr `elem` repeated then rest else instr : rest ) 
                [] 
                (instrs b)

replaceAddressesWithAddress :: [Maybe Address] -> Address -> LLVMInstr -> LLVMInstr
replaceAddressesWithAddress addrs addr og@(IBranch a l l') = if Just a `elem` addrs then IBranch addr l l' else og
replaceAddressesWithAddress addrs addr og@(ICmp s a1 a2)
  | Just a1 `elem` addrs && Just a2 `elem` addrs = ICmp s addr addr
  | Just a1 `elem` addrs = ICmp s addr a2
  | Just a2 `elem` addrs = ICmp s a1 addr
  | otherwise = og
replaceAddressesWithAddress addrs addr og@(IRet ty a) = if Just a `elem` addrs then IRet ty addr else og
replaceAddressesWithAddress addrs addr og@(IBinOp s a1 a2)
  | Just a1 `elem` addrs && Just a2 `elem` addrs = IBinOp s addr addr
  | Just a1 `elem` addrs = IBinOp s addr a2
  | Just a2 `elem` addrs = IBinOp s a1 addr
  | otherwise = og
replaceAddressesWithAddress addrs addr (IPhi ty phiRhs) = IPhi ty (map (\(a, l) -> if Just a `elem` addrs then (addr, l) else (a, l)) phiRhs)
replaceAddressesWithAddress addrs addr (ICall ty s args) = ICall ty s (map (\(ss, a) -> if Just a `elem` addrs then (ss, addr) else (ss, a)) args)
replaceAddressesWithAddress _ _ instr = instr 
