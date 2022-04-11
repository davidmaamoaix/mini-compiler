module Liveness where

import Control.Lens

import qualified Data.Set as S

import SSA

data LiveInfo = LiveInfo
    { lDef :: [Maybe RegId]
    , lLive :: [S.Set RegId]
    } deriving Show

-- Performs variable-oriented liveness analysis on the given SSA.
liveness :: [SSA] -> LiveInfo
liveness xs = LiveInfo def live
    where
        def = getDef <$> xs
        live = foldr (backtrack xs) (S.empty <$ xs) [0..length xs - 1]

-- Performs variable-oriented backtracking on the given line.
-- Updates the live set list with the backtracking results.
backtrack :: [SSA] -> Int -> [S.Set RegId] -> [S.Set RegId]
backtrack code line state = foldr (backtrackVar code line) state allLive
    where
        allLive = allUsed (code !! line)

backtrackVar :: [SSA] -> Int -> RegId -> [S.Set RegId] -> [S.Set RegId]
backtrackVar code (-1) reg state = state

usedInValue :: Value -> S.Set RegId
usedInValue (VLit _) = S.empty
usedInValue (VReg r) = S.singleton r

allUsed :: SSA -> S.Set RegId
allUsed (SMove _ s) = usedInValue s
allUsed (SNeg _ s) = usedInValue s
allUsed (SRet s) = usedInValue s
allUsed (SBinFunc _ _ a b) = usedInValue a `S.union` usedInValue b

getDef :: SSA -> Maybe RegId
getDef (SMove d _) = Just d
getDef (SNeg d _) = Just d
getDef (SRet _) = Nothing
getDef (SBinFunc d _ _ _) = Just d 
