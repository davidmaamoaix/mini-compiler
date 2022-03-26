{-# LANGUAGE GADTs #-}

module RegAlloc where

import Control.Lens
import Control.Monad.State.Lazy

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M

import Debug.Trace

import SSA

data LiveInfo = LiveInfo
    { lDef :: [Maybe RegId]
    , lLive :: [S.Set RegId]
    } deriving Show

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

greedyColor :: (Show c, LowBound c) => InterGraph c -> InterGraph c
greedyColor g = execState gState g
    where
        order = simpOrdering g
        update :: RegId -> c -> InterGraph c -> InterGraph c
        update i asn (IGraph n e c) = IGraph n e (M.insert i asn c)
        gState :: (Show c, LowBound c) => State (InterGraph c) ()
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

updateLiveInfo k d v (LiveInfo ds ls) = LiveInfo nd (ls & element k %~ S.insert v)
    where
        nd = ds & element k .~ d

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
