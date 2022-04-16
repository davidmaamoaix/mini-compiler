{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RegAlloc where

import GHC.Enum
import Data.Maybe (isJust, catMaybes)
import Data.Monoid
import Control.Lens
import Control.Monad.State.Lazy

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M

import SSA
import Liveness

import Debug.Trace

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

-- ColorState is the intermediate state of a register coloring algorithm
-- and includes all necessary components such as the interference graph, etc.
-- Note that some of the components are inter-dependent during the coloring
-- phrase (e.g. color of variables and the elimination ordering, due to pre-
-- coloring).
data ColorState c = CState
    { cOrd :: [RegId]
    , cWeights :: [Int]
    , cVerts :: S.Set RegId
    , cInter :: InterGraph
    -- , cColor :: M.Map RegId c
    }

makeLensesFor
    [ ("cOrd", "ordLens")
    , ("cWeights", "weightsLens")
    , ("cVerts", "vertsLens")
    , ("cInter", "interLens")
    ] ''ColorState

instance Enum AsmReg where
    fromEnum (Reg r) = fromEnum r
    fromEnum (Mem i) = nativeRegCount + i
    toEnum i = if i < nativeRegCount
        then Reg $ toEnum i
        else Mem (i - nativeRegCount)

instance LowBound AsmReg where
    lowest = flip lowestNotSeen 0

instance Precolor AsmReg where
    retReg = Just $ Reg RAX

-- Gets the lowest enum value that is not present in the given set.
lowestNotSeen :: (Enum a, Ord a) => S.Set a -> Int -> a
lowestNotSeen s i
    | S.member (toEnum i) s = lowestNotSeen s (i + 1)
    | otherwise = toEnum i

-- Colors the registers in an IR (respecting the precolors).
colorRegisters :: Precolor c => IR c -> M.Map RegId c
colorRegisters (IR n code precolor) = undefined

-- Lens for the set of interfering variables of a ColorState given
-- the target variable.
interRegsLens :: RegId -> Lens' (ColorState c) (S.Set RegId)
interRegsLens reg = interLens . edgesLens . at reg . non S.empty

-- Computation for precoloring a register to a color.
precolor :: RegId -> c -> Endo (ColorState c)
precolor reg color = Endo (& increWeights . removeVert)
    where
        increWeights :: ColorState c -> ColorState c
        increWeights s = s & weightsLens %~ increSetMap (s ^. interRegsLens reg)
        removeVert :: ColorState c -> ColorState c
        removeVert = vertsLens %~ S.delete reg

-- Increments all values in the weights map that corresponds to a
-- set of registers.
increSetMap :: S.Set Int -> [Int] -> [Int]
increSetMap s weights = incre <$> zip [0..] weights
    where
        incre :: (Int, Int) -> Int
        incre (idx, w) = if S.member w s then idx + 1 else idx

-- Greedily assigns color to the nodes with the given ordering.
greedyColoring :: LowBound c => InterGraph -> [RegId] -> M.Map RegId c
greedyColoring g xs = appEndo (greedyColoringEndo g xs) M.empty

-- Computation for greedy coloring.
greedyColoringEndo :: LowBound c => InterGraph -> [RegId] -> Endo (M.Map RegId c)
greedyColoringEndo g xs = mconcat $ colorVar g <$> xs

-- Maps a set with a list function.
listMapSet :: Ord b => ([a] -> [b]) -> S.Set a -> S.Set b
listMapSet f = S.fromList . f . S.toList

-- Gets all colors used for a given set of variables.
getSetColors :: Ord c => M.Map RegId c -> S.Set RegId -> S.Set c
getSetColors m set = listMapSet catMaybes $ S.map (`M.lookup` m) set

-- Computation for coloring a given variable.
colorVar :: LowBound c => InterGraph -> RegId -> Endo (M.Map RegId c)
colorVar (IGraph _ edges) var = Endo $ \m -> M.insert var (nextColor m) m
    where
        -- Gets the lowest color that is not used in its neighbors.
        nextColor :: LowBound c => M.Map RegId c -> c
        nextColor m = lowest (getSetColors m $ edges ^. at var . non S.empty)

-- Generates the simplicial elimination ordering for a given
-- interference graph.
simpOrdering :: InterGraph -> [RegId]
simpOrdering g@(IGraph n edges) = evalState (simpOrderingState g) initState
    where
        initState = CState [] (replicate n 0) (S.fromList [0..n - 1]) g

-- Computation for obtaining the simplicial elimination ordering.
simpOrderingState :: InterGraph -> State (ColorState c) [RegId]
simpOrderingState (IGraph n edges) = forM [1..n] $ \_ -> do
    -- Gets the node with maximum weight.
    maxV <- maxWeightNode
    -- Gets all interfering variables with the current node.
    neighbors <- nodeNeighbor edges maxV
    -- Removes the current variable from the unvisited set.
    vertsLens %= S.delete maxV
    -- Increments the weight of all interfering variables.
    forM_ neighbors $ \neighbor -> do
        weightsLens . ix neighbor += 1
    return maxV

-- State for getting the maximum weighted node to be assigned as
-- the next node in the elimination order.
maxWeightNode :: State (ColorState c) RegId
maxWeightNode = do
    w <- gets cWeights
    verts <- gets cVerts
    return $ L.maximumBy (\a b -> compare (w !! a) (w !! b)) verts

-- State for getting all interfering variables.
nodeNeighbor :: InterGraphEdges -> RegId -> State (ColorState c) (S.Set RegId)
nodeNeighbor edges r = return $ edges ^. at r . non S.empty
