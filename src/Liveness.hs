module Liveness where

import Control.Lens
import Data.Monoid

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
        live = appEndo (backtrack . reverse $ xs) (S.empty <$ xs)

-- Backtracks all live variables from the last line.
-- Note that the given code must be in reversed form.
backtrack :: [SSA] -> Endo [S.Set RegId]
backtrack [] = mempty
backtrack (x:xs) = backtrack xs <> mconcat (trackVar xs <$> allLive)
    where
        allLive = S.toList (allUsed x)

-- Provided with the variable to track, go back through all previous
-- lines and update the liveness state accordingly.
trackVar :: [SSA] -> RegId -> Endo [S.Set RegId]
trackVar code var = undefined

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
