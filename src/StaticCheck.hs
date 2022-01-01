{-# LANGUAGE GADTs #-}

module StaticCheck where

import Debug.Trace
import Control.Monad
import Control.Monad.State.Lazy
import qualified Data.Map as M

import Parser

type Var = Int

-- Corresponds to a binary operation to use in IR.
-- TODO: implment conversion to x86 translation.
data BinFunc
    = Add 
    | Sub
    | Mul
    | Div
    | Mod
    deriving (Eq, Show)

data SSA
    = SMove { dest :: Var, src :: Var }
    | SBinFunc { dest :: Var, func :: BinFunc, op1, op2 :: Var }
    deriving Show

data Env = Env
    { varCount :: Int
    , varRef :: M.Map String Int
    , code :: [SSA]
    } deriving Show

-- Adds a newly declared variable into the reference list
-- and returns the variable index for it.
addVar :: String -> State Env Int
addVar s = do
    count <- allocReg
    modify $ \(Env cnt ref ssa) -> Env cnt (M.insert s count ref) ssa
    return count

allocReg :: State Env Int
allocReg = do
    count <- gets varCount
    modify $ \(Env _ ref ssa) -> Env (count + 1) ref ssa
    return count


staticCheck :: Node Prog -> Maybe (Int, String)
staticCheck (NProg xs) = Nothing

-- Converts code to Static Single Assignment form.
-- TODO: change to target a function in future labs.
toSSA :: Node Prog -> [SSA]
toSSA (NProg xs) = code $ execState convert (Env 0 M.empty [])
    where
        convert :: State Env ()
        convert = forM_ xs stmtToSSA

stmtToSSA :: Node Stmt -> State Env ()
stmtToSSA (NBlockStmt xs) = forM_ xs stmtToSSA
stmtToSSA (NDeclStmt (NDecl s)) = void $ addVar s
stmtToSSA (NDeclStmt (NDeclAsn s exp)) = void $ addVar s
stmtToSSA (NSimpStmt _) = return ()
stmtToSSA (NRetStmt _) = return ()
