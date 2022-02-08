{-# LANGUAGE GADTs #-}

module RegAlloc where

import Control.Lens
import qualified Data.Set as S

import SSA

type LiveInfo = [S.Set RegId]

updateLiveInfo k v m = m & element k %~ S.insert v

liveness :: [SSA] -> LiveInfo
liveness xs = iter (S.empty <$ xs) $ reverse xs
    where
        iter :: [S.Set RegId] -> [SSA] -> [S.Set RegId]
        iter s [] = s
        iter s ls@(x:xs) = iter (foldr (propagate ls) s (allUsed x)) xs
        propagate :: [SSA] -> RegId -> [S.Set RegId] -> [S.Set RegId]
        propagate [] _ _ = []
        propagate (x:xs) r s = let (ns, cont) = curr in
                if cont then propagate xs r ns
                else ns
            where
                curr = if def x r
                    then (s, False)
                    else (updateLiveInfo (length xs) r s, True)

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
