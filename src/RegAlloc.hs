{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RegAlloc where

import GHC.Enum
import Control.Lens
import Control.Monad.State.Lazy

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M

import SSA
import Liveness

-- Native register names.
data RegName
    = RAX | RDI | RSI | RDX
    | RCX | R8 | R9 | R10 | R11 -- Caller-saved to this point.
    | RBX | R12 | R13 | R14
    | R15
    deriving (Show, Eq, Ord, Enum, Bounded)

-- Represents either a register or a memory location (spilled on stack).
data AsmReg
    = Reg RegName
    | Mem Int
    deriving (Show, Eq, Ord)

-- The amount of registers usable for register allocation (excluding
-- spilling onto stack).
nativeRegCount :: Int
nativeRegCount = fromEnum (maxBound :: RegName) + 1

-- Must have a way to get the lowest unused register.
class Ord c => LowBound c where
    lowest :: S.Set c -> c

data ColorState = CState
    { cOrd :: [RegId]
    , cWeights :: [Int]
    , cVerts :: S.Set RegId
    }

makeLensesFor
    [ ("cWeights", "weightsLens")
    , ("cVerts", "vertsLens")
    ] ''ColorState

instance Enum AsmReg where
    fromEnum (Reg r) = fromEnum r
    fromEnum (Mem i) = nativeRegCount + i
    toEnum i = if i < nativeRegCount
        then Reg $ toEnum i
        else Mem (i - nativeRegCount)

instance LowBound AsmReg where
    lowest = flip lowestNotSeen 0

-- Gets the lowest enum value that is not present in the given set.
lowestNotSeen :: (Enum a, Ord a) => S.Set a -> Int -> a
lowestNotSeen s i
    | S.member (toEnum i) s = lowestNotSeen s (i + 1)
    | otherwise = toEnum i

simpOrdering :: InterGraph c -> [RegId]
simpOrdering g@(IGraph n edges) = evalState (simpOrderingState g) initState
    where
        initState = CState [] (replicate n 0) (S.fromList [0..n - 1])

simpOrderingState :: InterGraph c -> State ColorState [RegId]
simpOrderingState (IGraph n edges) = forM [1..n] $ \_ -> do
    maxV <- maxWeightNode
    neighbors <- nodeNeighbor edges maxV
    vertsLens %= S.delete maxV
    forM_ neighbors $ \neighbor -> do
        weightsLens . ix neighbor += 1
    return maxV

maxWeightNode :: State ColorState RegId
maxWeightNode = do
    (CState _ w verts) <- get
    return $ L.maximumBy (\a b -> compare (w !! a) (w !! b)) verts

nodeNeighbor :: InterGraphEdges -> RegId -> State ColorState (S.Set RegId)
nodeNeighbor edges r = return $ edges ^. at r . non S.empty
