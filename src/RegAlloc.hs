{-# LANGUAGE GADTs #-}

module RegAlloc where

import Control.Lens
import Control.Monad.State.Lazy

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M

import SSA
import Liveness

data AsmReg
    = RAX | RBX | RCX | RDX
    | RSI | RDI | RBP | RSP
    | R08 | R09 | R10 | R11
    | R12 | R13 | R14 | R15
    deriving Show

data InterGraph c = IGraph
    { gNodes :: RegId
    , gEdges :: M.Map RegId (S.Set RegId)
    , gColor :: M.Map RegId c
    }

-- Must have a way to get the lowest unused register.
class Ord c => LowBound c where
    lowest :: S.Set c -> c

instance LowBound Int where
    lowest s = foldr (\a b -> if S.member a s then b else a) (-1) [0..]

data ColorState = CState
    { ord :: [RegId]
    , weights :: [Int]
    , verts :: S.Set RegId
    }

instance Functor InterGraph where
    fmap f (IGraph n e c) = IGraph n e (f <$> c)

greedyColor :: LowBound c => InterGraph c -> InterGraph c
greedyColor g = execState gState g
    where
        order = simpOrdering g
        update :: RegId -> c -> InterGraph c -> InterGraph c
        update i asn (IGraph n e c) = IGraph n e (M.insert i asn c)
        gState :: LowBound c => State (InterGraph c) ()
        gState = do
            forM_ order $ \i -> do
                g <- gets id
                let neigh = M.findWithDefault S.empty i $ gEdges g
                    asn = lowest $ used (gColor g) neigh
                modify $ update i asn
        used c s = S.fromList $ fmap snd $ filter (\(k, v) -> S.member k s) $ M.toList c

simpOrdering :: InterGraph c -> [RegId]
simpOrdering (IGraph n edges _) = reverse $ ord $ foldr iter initState [1..n]
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

genInterGraph :: IR -> InterGraph a
genInterGraph (IR n xs) = IGraph n genEdges M.empty
    where
        live = liveness xs
        genEdges = destLiveConf (conflict (lLive live)) live
        destLiveConf m (LiveInfo def live) = foldr conn m (zip def $ tail live)
        conn (Nothing, _) m = m
        conn (Just r, xs) m = foldr updateBoth m (zip (repeat r) $ S.toList xs)
        conflict l = foldr updateBoth M.empty $ l >>= (pairs . S.toList)
        updateBoth (a, b) m = if a /= b then update (a, b) (update (b, a) m) else m
        update (k, v) m = M.insertWith S.union k (S.singleton v) m
        pairs l = [(x, y) | (x:xs) <- L.tails l, y <- xs]
