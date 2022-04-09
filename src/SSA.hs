{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module SSA where

import Data.Maybe (isNothing, fromJust)
import Control.Monad
import Control.Monad.State.Lazy
import Control.Lens
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
    } deriving Show

makeLensesFor [("varCount", "countLens"), ("varRef", "refLens")] ''Env

-- Lens for the register corresponding to the given variable name.
varReg :: String -> Lens' Env (Maybe Int)
varReg name =  refLens . at name

-- State for allocating a register and incrementing the register counter.
allocReg :: State Env Int
allocReg = gets (^. countLens) <* (countLens += 1)

-- State for allocating a new register value to the given variable.
newReg :: String -> State Env Int
newReg name = do
    reg <- allocReg
    -- Set the corresponding register ID to a newly allocated one.
    varReg name <?= reg

move :: Value -> RegId -> State Env [SSA]
move val dest = return [SMove dest val]

-- State for generating code for loading an exp to a register.
loadToReg :: Node Exp -> RegId -> State Env [SSA]
loadToReg (NIntExp num) dest = move (VLit num) dest
loadToReg (NIdExp s) dest = do
    reg <- gets (^. varReg s)
    move (VReg $ fromJust reg) dest
loadToReg (NBinExp a op b) dest = do
    regA <- allocReg
    regB <- allocReg
    codeA <- loadToReg a regA
    codeB <- loadToReg b regB
    return $ codeA ++ codeB ++ [SBinFunc dest op (VReg regA) (VReg regB)]
loadToReg (NNegExp e) dest = do
    reg <- allocReg
    code <- loadToReg e reg
    return $ code ++ [SNeg dest (VReg reg)]

-- Converts code to Static Single Assignment form.
-- TODO: change to target a function in future labs.
toSSA :: Node Prog -> IR
toSSA (NProg xs) = IR (varCount $ snd result) (concat $ fst result)
    where
        result :: ([[SSA]], Env)
        result = runState (forM xs stmtToSSA) (Env 0 M.empty)

binOpCode :: String -> Node Exp -> BinOp -> State Env [SSA]
binOpCode name exp func = do
    src <- use $ varReg name
    new <- newReg name
    opReg <- allocReg
    code <- loadToReg exp opReg
    return [SBinFunc new func (VReg $ fromJust src) (VReg opReg)]

-- State for converting a statement into SSA form.
stmtToSSA :: Node Stmt -> State Env [SSA]
stmtToSSA (NBlockStmt xs) = concat <$> forM xs stmtToSSA
stmtToSSA (NSimpStmt (NSimp (NIdL l) op exp)) = let bin = binOpCode l exp in
    case op of
        AddAsn -> bin Add
        SubAsn -> bin Sub
        MulAsn -> bin Mul
        DivAsn -> bin Div
        ModAsn -> bin Mod
        Asn -> newReg l >>= loadToReg exp
stmtToSSA (NRetStmt exp) = do
    reg <- allocReg
    code <- loadToReg exp reg
    return $ code ++ [SRet (VReg reg)]
stmtToSSA (NDeclStmt d) = case d of 
    NDecl _ -> return []
    NDeclAsn s exp -> newReg s >>= loadToReg exp
