{-# LANGUAGE GADTs #-}

module StaticCheck where

import Parser

type Var = String

-- Corresponds to a binary operation to use in IR.
-- TODO: implment conversion to x86 translation.
data BinFunc
    = Add 
    | Sub
    | Mul
    | Div
    | Mod

data SSA
    = SMove { dest :: Var, src :: Var }
    | SBinFunc { dest :: Var, op1, op2 :: Var, func :: BinFunc }

staticCheck :: Node Prog -> Maybe (Int, String)
staticCheck (NProg xs) = Nothing

-- Converts a function to Static Single Assignment form.
-- TODO: change to function in future labs.
toSSA :: Node Prog -> [SSA]
toSSA = undefined
