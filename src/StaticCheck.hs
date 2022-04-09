{-# LANGUAGE GADTs #-}

module StaticCheck where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Functor ((<&>))
import Data.Functor.Identity
import qualified Data.Map.Strict as M

import Parser

data VarState
    = VUnseen
    | VDecl
    | VInit
    deriving (Eq, Show)

type FlowEnv = M.Map String VarState
type FlowState a = ExceptT String (State FlowEnv) a

unseenMsg = "Undeclared variable: "
notInitMsg = "Variable is not initialized when used: "

getVarState :: String -> FlowState VarState
getVarState name = lift get <&> M.findWithDefault VUnseen name

setVarState :: String -> VarState -> FlowState ()
setVarState var st = modify $ M.insert var st

-- A state that fails if the "variable state" of the given variable
-- fails the given predicate.
assertVarState :: String -> (VarState -> Bool) -> String -> FlowState ()
assertVarState name p errMsg = do
    state <- getVarState name
    unless (p state) $ throwError (errMsg ++ name)

assertDecl :: String -> FlowState ()
assertDecl s = assertVarState s (/= VUnseen) unseenMsg

assertInit :: String -> FlowState ()
-- Calls "assertDecl" for undeclared error message.
assertInit s = assertDecl s *> assertVarState s (== VInit) notInitMsg

staticCheck :: Node Prog -> Either String FlowEnv
staticCheck (NProg xs) = do
    let (err, out) = runIdentity result
    case err of
        Left "" -> Right out
        Right env -> error $ show out
        _ -> out <$ err
    where
        result = (runStateT $ runExceptT $ controlFlow True xs) M.empty

-- mustRet :: Bool denotes whether an error is thrown if an empty list
-- is encountered without short-wired by a return statement (indicating
-- missing return).
controlFlow :: Bool -> [Node Stmt] -> FlowState ()
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
