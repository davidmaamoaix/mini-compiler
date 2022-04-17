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

-- State computation for code emitting.
codeGenComp :: State CodeGenState [String]
codeGenComp = undefined
