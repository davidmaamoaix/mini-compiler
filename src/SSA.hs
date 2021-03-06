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

data IR c = IR
    { irVars :: Int
    , irCode :: [SSA]
    , irPrecolor :: M.Map RegId c
    } deriving Show

data Env c = Env
    { varCount :: Int
    , varRef :: M.Map String Int
    , varColor :: M.Map Int c -- precolors certain IR registers
    } deriving Show

makeLensesFor
    [ ("varCount", "countLens")
    , ("varRef", "refLens")
    , ("varColor", "precolorLens")
    ] ''Env

-- An register type must provide some target language dependent
-- register info, in this case, the fixed colors that some operations
-- use, e.g. %EAX for return value in x86-64. If the target language
-- does not specify a fixed register for an operation, simply assign
-- 'Nothing' to it.
class Precolor c where
    retReg :: Maybe c

-- Lens for the register corresponding to the given variable name.
varReg :: String -> Lens' (Env c) (Maybe Int)
varReg name =  refLens . at name

-- State for allocating a register and incrementing the register counter.
allocReg :: State (Env c) Int
allocReg = gets (^. countLens) <* (countLens += 1)

-- State for allocating a new register value to the given variable.
newReg :: String -> State (Env c) Int
newReg name = do
    reg <- allocReg
    -- Sets the corresponding register ID to a newly allocated one.
    varReg name <?= reg

move :: Value -> RegId -> State (Env c) [SSA]
move val dest = return [SMove dest val]

-- State for generating code for loading an exp to a register.
loadToReg :: Node Exp -> RegId -> State (Env c) [SSA]
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
toSSA :: Precolor c => Node Prog -> IR c
toSSA (NProg xs) = IR (varCount env) (concat code) (varColor env)
    where
        -- result :: Precolor c => ([[SSA]], Env c)
        (code, env) = runState (forM xs stmtToSSA) (Env 0 M.empty M.empty)

binOpCode :: String -> Node Exp -> BinOp -> State (Env c) [SSA]
binOpCode name exp func = do
    src <- use $ varReg name
    new <- newReg name
    opReg <- allocReg
    code <- loadToReg exp opReg
    return $ code ++ [SBinFunc new func (VReg $ fromJust src) (VReg opReg)]

-- State for precoloring a register. Note that the colored register should
-- have limited liveness range to eliminate graph interference (e.g. should
-- be a temporary that is only assigned before the instruction with machine-
-- specific register and cannot be live afterwards).
colorReg :: RegId -> Maybe c -> State (Env c) ()
colorReg _ Nothing = return ()
colorReg reg (Just color) = precolorLens %= M.insert reg color

-- State for converting a statement into SSA form.
stmtToSSA :: Precolor c => Node Stmt -> State (Env c) [SSA]
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
    colorReg reg retReg
    code <- loadToReg exp reg
    return $ code ++ [SRet (VReg reg)]
stmtToSSA (NDeclStmt d) = case d of 
    NDecl _ -> return []
    NDeclAsn s exp -> newReg s >>= loadToReg exp
