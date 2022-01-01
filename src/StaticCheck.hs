{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module StaticCheck where

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

data SSA
    = SMove { dest :: Var, src :: Var }
    | SBinFunc { dest :: Var, func :: BinFunc, op1, op2 :: Var }

data Env = Env
    { varCount :: Int
    , varRef :: M.Map String Int
    , code :: [SSA]
    }

-- Typeclass that matches GADT nodes based on whether
-- its SSA form has a return register.
class ConvertSSA a o | a -> o where
    conv :: State Env o

staticCheck :: Node Prog -> Maybe (Int, String)
staticCheck (NProg xs) = Nothing

-- Converts code to Static Single Assignment form.
-- TODO: change to target a function in future labs.
toSSA :: Node Prog -> [SSA]
toSSA x = undefined
