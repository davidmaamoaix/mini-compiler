{-# LANGUAGE GADTs #-}

module StaticCheck where

import Debug.Trace
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

data Env = Env
    { varCount :: Int
    , varRef :: M.Map String Int
    , code :: [SSA]
    } deriving Show

-- Gets the register ID corresponding to a variable. Note
-- that each assignment to variable creates a new register.
-- Creats the register if it does not exist.
getVarReg :: String -> Bool -> State Env Int
getVarReg s fresh = do
    env <- get
    let refMap = varRef env
    if M.member s refMap && not fresh
        then return (refMap M.! s)
        else do
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
        convert = forM_ (reverse xs) stmtToSSA

stmtToSSA :: Node Stmt -> State Env ()
stmtToSSA (NBlockStmt xs) = forM_ (reverse xs) stmtToSSA
stmtToSSA (NSimpStmt (NSimp (NIdL l) Asn exp)) = do
    reg <- getVarReg l True
    loadToReg exp reg
    return ()
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
    modify $ \(Env cnt ref ssa) -> Env cnt ref (SBinFunc new func (VReg src) (VReg opReg) : ssa)
stmtToSSA (NRetStmt exp) = do
    reg <- allocReg
    loadToReg exp reg
    modify $ \(Env cnt ref ssa) -> Env cnt ref (SRet (VReg reg) : ssa)
    return ()
stmtToSSA (NDeclStmt (NDecl s)) = return ()
stmtToSSA (NDeclStmt (NDeclAsn s exp)) = do
    reg <- getVarReg s True
    loadToReg exp reg
    return ()

-- Generates SSA code that loads the evaluated value of
-- an Exp node to a given register.
-- TODO: lens the whole "editing environment" thingy and add optimization rules
loadToReg :: Node Exp -> RegId -> State Env ()
loadToReg (NIntExp num) dest = modify $ \(Env cnt ref ssa) -> Env cnt ref (SMove dest (VLit num) : ssa)
loadToReg (NIdExp s) dest = do
    reg <- getVarReg s False
    modify $ \(Env cnt ref ssa) -> Env cnt ref (SMove dest (VReg reg) : ssa)
loadToReg (NBinExp a op b) dest = do
    regA <- allocReg
    regB <- allocReg
    loadToReg a regA
    loadToReg b regB
    modify $ \(Env cnt ref ssa) -> Env cnt ref (SBinFunc dest op (VReg regA) (VReg regB) : ssa)
loadToReg (NNegExp e) dest = do
    reg <- allocReg
    loadToReg e reg
    modify $ \(Env cnt ref ssa) -> Env cnt ref (SNeg dest (VReg reg) : ssa)
