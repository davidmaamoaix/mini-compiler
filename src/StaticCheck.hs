{-# LANGUAGE GADTs #-}

module StaticCheck where

import Debug.Trace
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
    unless (p $ M.findWithDefault VUnseen var env)
        $ throwError (errMsg ++ var)

assertDecl :: String -> FlowState
assertDecl s = assertVarState s (/= VUnseen) "Undeclared variable: "

assertInit :: String -> FlowState
assertInit s = assertDecl s *> assertVarState s (== VInit) msg
    where
        msg = "Variable is not initialized when used: "

setVarState :: String -> VarState -> FlowState
setVarState var st = modify $ M.insert var st

staticCheck :: Node Prog -> Either String FlowEnv
staticCheck (NProg xs) = do
    let (err, out) = runIdentity result
    case err of
        Left "" -> Right out
        Right env -> error $ show out
        _ -> out <$ err
    where
        result = runStateT (runExceptT (controlFlow True xs)) M.empty

-- mustRet :: Bool denotes whether an error is thrown if
-- an empty list is encountered without short-wiring by a
-- return statement (indicating missing return).
controlFlow :: Bool -> [Node Stmt] -> FlowState
controlFlow True [] = throwError "No return statement in control flow"
controlFlow False [] = return ()
controlFlow mustRet (x:xs) = do
    case x of
        NBlockStmt code -> controlFlow False code
        NDeclStmt (NDecl v) -> setVarState v VDecl
        NRetStmt exp -> do
            forM_ (getVarsFromExp exp) assertInit
            throwError "" -- quick hack: empty error = success
        NSimpStmt (NSimp (NIdL v) _ exp) -> do
            assertDecl v
            forM_ (getVarsFromExp exp) assertInit
            setVarState v VInit
        NDeclStmt (NDeclAsn v exp) -> do
            forM_ (getVarsFromExp exp) assertInit
            setVarState v VInit
    controlFlow mustRet xs

getVarsFromExp :: Node Exp -> [String]
getVarsFromExp (NIntExp _) = []
getVarsFromExp (NIdExp s) = [s]
getVarsFromExp (NBinExp l _ r) = getVarsFromExp l ++ getVarsFromExp r
getVarsFromExp (NNegExp exp) = getVarsFromExp exp
