{-# LANGUAGE GADTs #-}

module RegAlloc where

import Control.Lens
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M

import SSA

data LiveInfo = LiveInfo
    { lDef :: [Maybe RegId]
    , lLive :: [S.Set RegId]
    } deriving Show

data AsmReg
    = RAX | RBX | RCX | RDX
    | RSI | RDI | RBP | RSP
    | R08 | R09 | R10 | R11
    | R12 | R13 | R14 | R15
    deriving Show

data InterGraph c = IGraph
    { gNodes :: Int
    , gEdges :: M.Map Int (S.Set Int)
    , gColor :: M.Map Int c
    }

instance Functor InterGraph where
    fmap f (IGraph n e c) = IGraph n e (f <$> c)

updateLiveInfo k d v (LiveInfo ds ls) = LiveInfo nd (ls & element k %~ S.insert v)
    where
        nd = ds & element k .~ d

-- TODO: dest at l interferes with live at l+1
genInterGraph :: IR -> InterGraph a
genInterGraph (IR n xs) = IGraph n (genEdges xs) M.empty
    where
        genEdges xs = conflict (lLive $ liveness xs)
        conflict l = foldr updateBoth M.empty $ l >>= (pairs . S.toList)
        updateBoth (a, b) m = update (a, b) (update (b, a) m)
        update (k, v) m = M.insertWith S.union k (S.singleton v) m
        pairs l = [(x, y) | (x:xs) <- L.tails l, y <- xs]

liveness :: [SSA] -> LiveInfo
liveness xs = iter (LiveInfo (Nothing <$ xs) (S.empty <$ xs)) $ reverse xs
    where
        iter :: LiveInfo -> [SSA] -> LiveInfo
        iter s [] = s
        iter s ls@(x:xs) = iter (foldr (propagate ls) s (allUsed x)) xs
        propagate :: [SSA] -> RegId -> LiveInfo -> LiveInfo
        propagate [] _ l = l
        propagate (x:xs) r i@(LiveInfo d l) = let (ns, cont) = curr in
                if cont then propagate xs r ns
                else ns
            where
                curr = let currDef = getDef x in
                    if Just r == currDef
                    then (LiveInfo (d & element (length xs) ?~ r) l, False)
                    else (updateLiveInfo (length xs) currDef r i, True)

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

use :: SSA -> RegId -> Bool
use (SMove _ s) r = S.member r (usedInValue s)
use (SNeg _ s) r = S.member r (usedInValue s)
use (SRet s) r = S.member r (usedInValue s)
use (SBinFunc _ _ a b) r = S.member r set
    where
        set = usedInValue a `S.union` usedInValue b

def :: SSA -> RegId -> Bool
def (SMove d _) r = d == r
def (SNeg d _) r = d == r
def (SRet _) _ = False
def (SBinFunc d _ _ _) r = d == r
