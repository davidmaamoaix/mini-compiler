module Liveness where

import Control.Lens

import qualified Data.Set as S

import SSA

data LiveInfo = LiveInfo
    { lDef :: [Maybe RegId]
    , lLive :: [S.Set RegId]
    } deriving Show

updateLiveInfo k d v (LiveInfo ds ls) = LiveInfo nd (ls & element k %~ S.insert v)
    where
        nd = ds & element k .~ d

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
