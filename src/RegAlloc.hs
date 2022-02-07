{-# LANGUAGE GADTs #-}

module RegAlloc where

import SSA

data Live

liveness :: [SSA] -> Live
liveness = undefined

liveInValue :: Value -> [RegId]
liveInValue (VLit _) = []
liveInValue (VReg r) = [r]

liveIn :: SSA -> [RegId]
liveIn (SMove _ s) = liveInValue s
liveIn (SNeg _ s) = liveInValue s
liveIn (SRet s) = liveInValue s
liveIn (SBinFunc _ _ a b) = liveInValue a ++ liveInValue b
