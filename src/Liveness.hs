{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Liveness where

import Debug.Trace

import Data.Maybe
import Data.Monoid
import Control.Lens
import Control.Monad (guard)

import qualified Data.Set as S
import qualified Data.Map as M

import SSA

data LiveInfo = LiveInfo
    { lDef :: [Maybe RegId]
    , lLive :: [S.Set RegId]
    } deriving Show

-- Inserts the register into the set at the given index of the
-- liveness state.
insertLive :: Int -> RegId -> Endo [S.Set RegId]
insertLive index reg = Endo (& ix index %~ S.insert reg)

-- Performs variable-oriented liveness analysis on the given SSA.
liveness :: [SSA] -> LiveInfo
liveness xs = LiveInfo def live
    where
        def = getDef <$> xs
        live = appEndo (backtrack . reverse $ xs) (S.empty <$ xs)

-- Backtracks all live variables from the last line.
-- Note that the given code must be in reversed form.
backtrack :: [SSA] -> Endo [S.Set RegId]
backtrack [] = mempty
backtrack l@(x:xs) = backtrack xs <> mconcat (startTrack <$> allLive)
    where
        startTrack r = trackVar xs r <> insertLive (length xs) r
        allLive = S.toList (allUsed x)

-- Provided with the variable to track, go back through all previous
-- lines and update the liveness state accordingly.
trackVar :: [SSA] -> RegId -> Endo [S.Set RegId]
trackVar [] var = mempty
trackVar (x:xs) var
    | getDef x == Just var = mempty
    | otherwise = trackVar xs var <> insertLive (length xs) var

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

type InterGraphEdges = M.Map RegId (S.Set RegId)

-- Interference graph.
data InterGraph = IGraph
    { gNodes :: RegId
    , gEdges :: InterGraphEdges
    }

makeLensesFor [("gEdges", "edgesLens")] ''InterGraph

genInterGraph :: Int -> LiveInfo -> InterGraph
genInterGraph regCount l = IGraph regCount edges
    where
        edges :: InterGraphEdges
        edges = appEndo (interGraphComp l) M.empty

-- Computation that generates the interference graph based on
-- the given liveness information.
interGraphComp :: LiveInfo -> Endo InterGraphEdges
interGraphComp l = asnLiveInter l <> mconcat (genInterFromLine <$> lLive l)

-- Gets the cartesian product of two sets with no pairs containing
-- the same element twice.
cartPairs :: Eq a => S.Set a -> S.Set a -> [(a, a)]
cartPairs sa sb = do
    a <- S.toList sa
    b <- S.toList sb
    guard $ a /= b
    return (a, b)

-- Updates the interference graph with a key-value pair (the symmetric
-- inverse is not added though).
updatePair :: (RegId, RegId) -> Endo InterGraphEdges
updatePair (a, b) = Endo (& at a . non S.empty %~ S.insert b)

updateAllPairs :: [(RegId, RegId)] -> Endo InterGraphEdges
updateAllPairs = mconcat . (updatePair <$>)

-- Computation that populates an interference graph with live
-- variables in each statement.
genInterFromLine :: S.Set RegId -> Endo InterGraphEdges
genInterFromLine s = updateAllPairs $ cartPairs s s

-- The variable assigned to at L interferes with all live variables
-- at L + 1.
asnLiveInter :: LiveInfo -> Endo InterGraphEdges
asnLiveInter (LiveInfo def live) = updateAllPairs $ concat (genPairs <$> zipped)
    where
        -- Zips definition at L with live variables at L + 1.
        zipped :: [(RegId, S.Set RegId)]
        zipped = [(fromJust a, b) | (a, b) <- zip def (tail live), isJust a]
        genPairs :: Eq a => (a, S.Set a) -> [(a, a)]
        genPairs (a, s) = cartPairs (S.singleton a) s ++ cartPairs s (S.singleton a)
