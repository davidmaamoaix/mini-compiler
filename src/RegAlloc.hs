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
    , cNoColor :: S.Set RegId
    , cInter :: InterGraph
    , cColor :: M.Map RegId c
    }

makeLensesFor
    [ ("cOrd", "ordLens")
    , ("cWeights", "weightsLens")
    , ("cNoColor", "noColorLens")
    , ("cInter", "interLens")
    , ("cColor", "colorLens")
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
colorRegisters :: LowBound c => IR c -> M.Map RegId c
colorRegisters ir = evalState colorFromInitial (initColorState ir)

-- Computation for processing an initial state and returns the final
-- variable color assignment.
colorFromInitial :: LowBound c => State (ColorState c) (M.Map RegId c)
colorFromInitial = do
    -- Populates simplicial elimination ordering.
    simpOrdering
    -- Colors the remaining uncolored nodes.
    greedyColoringState
    -- Returns final coloring.
    gets cColor

initColorState :: IR c -> ColorState c
initColorState (IR n code precolor) = CState [] weights unColored inter precolor
    where
        -- Initial interference graph.
        inter :: InterGraph
        inter = genInterGraph n $ liveness code
        -- Set of precolored nodes.
        precolored :: S.Set RegId
        precolored = M.keysSet precolor
        -- Set of uncolored nodes.
        unColored :: S.Set RegId
        unColored = S.fromList [0..n-1] `S.difference` precolored
        -- Initial weight of a node = neighboring nodes that are colored.
        weights :: [Int]
        weights = [weight x | x <- [0..n-1]]
        -- Gets the initial weight of a single variable.
        weight :: RegId -> Int
        weight reg = S.size $ (inter ^. interSetLens reg) `S.intersection` precolored

-- Lens for the set of interfering variables of a ColorState given
-- the target variable.
interRegsLens :: RegId -> Lens' (ColorState c) (S.Set RegId)
interRegsLens reg = interLens . interSetLens reg

-- Computation for precoloring a register to a color.
precolor :: RegId -> c -> State (ColorState c) ()
precolor reg color = do
    -- Removes the register from the uncolored set.
    noColorLens %= S.delete reg
    -- Gets neighbors.
    neighbors <- gets (^. interRegsLens reg)
    -- Increments all neighboring weights.
    forM_ neighbors $ \s -> do
        weightsLens . ix s += 1

-- State for greedy coloring the rest of uncolored odes.
greedyColoringState :: LowBound c => State (ColorState c) ()
greedyColoringState = do
    -- Gets the elimination ordering.
    ordering <- gets cOrd
    -- Since all nodes in the ordering are uncolored, just map
    -- through the states of coloring each of them.
    forM_ ordering $ \reg -> do
        -- Greedily assigns color.
        newColor <- getGreedyColor reg
        colorLens . at reg ?= newColor

-- Maps a set with a list function.
listMapSet :: Ord b => ([a] -> [b]) -> S.Set a -> S.Set b
listMapSet f = S.fromList . f . S.toList

-- Gets all colors used for a given variable's neighborhood.
getNeighborhoodColors :: Ord c => RegId -> State (ColorState c) (S.Set c)
getNeighborhoodColors reg = do
    -- Gets all interfering variables.
    conflicts <- gets (^. interRegsLens reg)
    -- Gets all current colors.
    colors <- gets cColor
    -- Maps the whole conflict set across the color map.
    return $ listMapSet catMaybes $ S.map (`M.lookup` colors) conflicts
    
-- listMapSet catMaybes $ S.map (`M.lookup` m) set

-- Computation for getting the lowest unused color in the neighborhood
-- formed by the given variable.
getGreedyColor :: LowBound c => RegId -> State (ColorState c) c
getGreedyColor reg = do
    used <- getNeighborhoodColors reg
    return $ lowest used

-- Populates the simplicial elimination ordering field from the remaining
-- uncolored variables. Does NOT contain precolored nodes.
simpOrdering :: State (ColorState c) ()
simpOrdering = genSimpOrdering >>= (ordLens .=)

-- Computation for obtaining the simplicial elimination ordering.
genSimpOrdering :: State (ColorState c) [RegId]
genSimpOrdering = do
    unvisited <- gets cNoColor
    forM (S.toList unvisited) $ \_ -> do
        -- Gets the node with maximum weight.
        maxV <- maxWeightNode
        -- Gets all interfering variables with the current node.
        neighbors <- gets (^. interRegsLens maxV)
        -- Removes the current variable from the uncolored set.
        noColorLens %= S.delete maxV
        -- Increments the weight of all interfering variables.
        forM_ neighbors $ \neighbor -> do
            weightsLens . ix neighbor += 1
        return maxV

-- State for getting the maximum weighted node to be assigned as
-- the next node in the elimination order.
maxWeightNode :: State (ColorState c) RegId
maxWeightNode = do
    w <- gets cWeights
    verts <- gets cNoColor
    return $ L.maximumBy (\a b -> compare (w !! a) (w !! b)) verts
