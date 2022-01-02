{-# LANGUAGE GADTs #-}

module StaticCheck where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Functor.Identity
import qualified Data.Map.Strict as M

import Parser

data VarState
    = VUnseen
    | VDecl
    | VInit
    deriving (Eq, Show)

type FlowEnv = M.Map String VarState
type FlowState = ExceptT String (State FlowEnv) ()

assertVarState :: String -> (VarState -> Bool) -> String -> FlowState
assertVarState var p errMsg = do
    env <- lift get
    when (p $ M.findWithDefault VUnseen var env)
        $ throwError (errMsg ++ var)

assertSeen :: String -> FlowState
assertSeen s = assertVarState s (/= VUnseen) "Undeclared variable: "

assertInit :: String -> FlowState
assertInit s = assertVarState s (== VInit) msg
    where
        msg = "Variable is not initialized when used: "

staticCheck :: Node Prog -> Either String FlowEnv
staticCheck (NProg xs) = do
    let (err, out) = runIdentity result
    out <$ err
    where
        result = runStateT (runExceptT (controlFlow xs)) M.empty

controlFlow :: [Node Stmt] -> FlowState
controlFlow [] = throwError "No return statement in control flow"
controlFlow (x:xs) = case x of
    NBlockStmt code -> controlFlow code
    NDeclStmt (NDecl v) -> error ""
