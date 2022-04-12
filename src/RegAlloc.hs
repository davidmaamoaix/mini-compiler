{-# LANGUAGE GADTs #-}
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
    { ord :: [RegId]
    , weights :: [Int]
    , verts :: S.Set RegId
    }

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
simpOrdering (IGraph n edges) = reverse $ ord $ foldr iter initState [1..n]
    where
        iter _ s@(CState ord w v) = insertMax (maxVert w v) s
        insertMax t (CState ord w v) = CState (t:ord) (updateW v w t) (S.delete t v)
        updateW nodes weights t = foldr (\a w -> w & element a %~ (+ 1)) weights inter
            where
                inter = S.toList $ S.intersection nodes connections
                connections = M.findWithDefault S.empty t edges
        initState = CState [] initWeights initVerts
        initWeights = replicate n 0
        initVerts = S.fromList [0..n-1]
        maxVert w v = L.maximumBy (\a b -> compare (w !! a) (w !! b)) v
