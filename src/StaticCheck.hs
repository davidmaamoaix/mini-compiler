{-# LANGUAGE GADTs #-}

module StaticCheck where

import Data.Functor.Identity
import Control.Monad.State.Lazy
import Control.Monad.Except
import qualified Data.Map as M

import Parser

data VarState
    = VUnseed
    | VDecl
    | VInit

type FlowEnv = M.Map String VarState

staticCheck :: Node Prog -> Either String FlowEnv
staticCheck (NProg xs) = do
    let (err, out) = runIdentity result
    out <$ err
    where
        result = runStateT (runExceptT (controlFlow xs)) M.empty

controlFlow :: [Node Stmt] -> ExceptT String (State FlowEnv) ()
controlFlow = undefined
--controlFlow [] = Left "No return statement in control flow"
--controlFlow (x:xs) = case x of
--    NBlockStmt code -> controlFlow code
--    NDeclStmt (NDecl v) -> error ""
