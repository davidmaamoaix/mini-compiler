module CodeGen where

import Control.Lens
import Control.Monad.State.Lazy

import qualified Data.Set as S
import qualified Data.Map as M

import SSA
import RegAlloc

data CodeGenState = CGenState
    { gCode :: [SSA]
    , gColors :: M.Map RegId AsmReg
    }

-- Generates x86-64 assembly for a given code in SSA form.
codeGen :: [SSA] -> M.Map RegId AsmReg -> [String]
codeGen code colors = evalState codeGenComp (CGenState code colors)

-- Cuz looks are important YAY!
tab :: String -> String
tab = ("    " ++)

-- State computation for code emitting.
codeGenComp :: State CodeGenState [String]
codeGenComp = do
    let header = ".text"
        mainHeader = "main:"
    start <- asmEntry "main"
    return $ (header : start) ++ (mainHeader : [])

-- Generates code for setting up the entry point to the given label name.
asmEntry :: String -> State CodeGenState [String]
asmEntry entry = return
    [ ".globl _start"
    , "_start:"
    , tab "call " ++ entry
    , tab "movl %eax, %edi"
    , tab "movl $60, %eax"
    , tab "syscall"
    ]