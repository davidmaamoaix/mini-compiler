{-# LANGUAGE GADTs #-}

module SSA where

import Data.Maybe (isNothing)
import Control.Monad
import Control.Monad.State.Lazy
import qualified Data.Map as M

import Parser

type RegId = Int
data Value
    = VLit Integer
    | VReg RegId
    deriving (Eq, Show)

data SSA
    = SMove { dest :: RegId, src :: Value }
    | SNeg { dest :: RegId, src :: Value }
    | SRet { src :: Value }
    | SBinFunc { dest :: RegId, func :: BinOp, op1, op2 :: Value }
    deriving Show

data IR = IR { irVars :: Int, irCode :: [SSA] } deriving Show

data Env = Env
    { varCount :: Int
    , varRef :: M.Map String Int
    , code :: [SSA]
    } deriving Show

-- TODO: lens this
appendSSA :: SSA -> State Env ()
appendSSA s = modify $ \(Env cnt ref ssa) -> Env cnt ref (s : ssa)

-- Gets the register ID corresponding to a variable. Note
-- that each assignment to variable creates a new register.
-- Creats the register if it does not exist.
getVarReg :: String -> Bool -> State Env Int
getVarReg s fresh = do
    refMap <- gets varRef
    let regId = M.lookup s refMap
    maybe updateReg (\r -> if fresh then updateReg else return r) regId
    where
        updateReg = do
            count <- allocReg
            modify $ \(Env cnt ref ssa) -> Env cnt (M.insert s count ref) ssa
            return count

allocReg :: State Env Int
allocReg = do
    count <- gets varCount
    modify $ \(Env _ ref ssa) -> Env (count + 1) ref ssa
    return count

-- Converts code to Static Single Assignment form.
-- TODO: change to target a function in future labs.
toSSA :: Node Prog -> IR
toSSA (NProg xs) = IR (varCount env) (reverse . code $ env)
    where
        env = execState convert (Env 0 M.empty [])
        convert :: State Env ()
        convert = forM_ xs stmtToSSA

stmtToSSA :: Node Stmt -> State Env ()
stmtToSSA (NBlockStmt xs) = forM_ xs stmtToSSA
stmtToSSA (NSimpStmt (NSimp (NIdL l) Asn exp)) = do
    reg <- getVarReg l True
    loadToReg exp reg
stmtToSSA (NSimpStmt (NSimp (NIdL l) op exp)) = do
    src <- getVarReg l False
    new <- getVarReg l True
    opReg <- allocReg
    loadToReg exp opReg
    let func = case op of
            AddAsn -> Add
            SubAsn -> Sub
            MulAsn -> Mul
            DivAsn -> Div
            ModAsn -> Mod
            Asn -> error "impossible"
    appendSSA $ SBinFunc new func (VReg src) (VReg opReg)
stmtToSSA (NRetStmt exp) = do
    reg <- allocReg
    loadToReg exp reg
    appendSSA $ SRet (VReg reg)
stmtToSSA (NDeclStmt (NDecl s)) = return ()
stmtToSSA (NDeclStmt (NDeclAsn s exp)) = do
    reg <- getVarReg s True
    loadToReg exp reg

-- Generates SSA code that loads the evaluated value of
-- an Exp node to a given register.
loadToReg :: Node Exp -> RegId -> State Env ()
loadToReg (NIntExp num) dest = appendSSA $ SMove dest (VLit num)
loadToReg (NIdExp s) dest = do
    reg <- getVarReg s False
    appendSSA $ SMove dest (VReg reg)
loadToReg (NBinExp a op b) dest = do
    regA <- allocReg
    regB <- allocReg
    loadToReg a regA
    loadToReg b regB
    appendSSA $ SBinFunc dest op (VReg regA) (VReg regB)
loadToReg (NNegExp e) dest = do
    reg <- allocReg
    loadToReg e reg
    appendSSA $ SNeg dest (VReg reg)