{-# LANGUAGE GADTs #-}

module RegAlloc where

import qualified Data.Set as S

import SSA

type LiveInfo = [S.Set RegId]

liveness :: [SSA] -> LiveInfo
liveness xs = iter (S.empty <$ xs) $ reverse xs
    where
        iter :: [S.Set RegId] -> [SSA] -> [S.Set RegId]
        iter s [] = s
        iter s (x:xs) = iter (foldr (propagate xs) s (allUsed x)) xs
        propagate :: [SSA] -> RegId -> [S.Set RegId] -> [S.Set RegId]
        propagate [] _ _ = []
        propagate (x:xs) r s = undefined


usedInValue :: Value -> S.Set RegId
usedInValue (VLit _) = S.empty
usedInValue (VReg r) = S.singleton r

allUsed :: SSA -> S.Set RegId
allUsed (SMove _ s) = usedInValue s
allUsed (SNeg _ s) = usedInValue s
allUsed (SRet s) = usedInValue s
allUsed (SBinFunc _ _ a b) = usedInValue a `S.union` usedInValue b

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
