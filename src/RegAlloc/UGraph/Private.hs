module RegAlloc.UGraph.Private where

import Prelude hiding (null)
import Control.Monad ((>=>))
import Data.Foldable (fold)
import qualified Data.Foldable as Foldable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Map.Class (Union (..), (!?))
import qualified Data.Map.Class as Map

import qualified RegAlloc.Nodes as Nodes
import RegAlloc.Nodes.Private

newtype UGraph = UGr { unUGr :: Union IntMap IntSet }
  deriving (Eq, Show)

null, nullEdges :: UGraph -> Bool
null = Foldable.null . unUGr
nullEdges = (all . \ f -> all f . IS.toList) (< 0) . unUGr

(!) :: UGraph -> Int -> Nodes
(!) = flip nbrsOf

nbrsOf :: Int -> UGraph -> Nodes
nbrsOf k
  | k < 0 = pure (Nodes mempty)
  | True  = Nodes . fold . (!? k) . unUGr

coalesce, coalesceIfNoEdge :: Node -> Int -> UGraph -> UGraph
coalesce (Node_ a) b =
    UGr . fmap (replaceIS b a) . Map.delete b .
    (\ gr -> Map.adjust (maybe id (<>) (gr Map.!? b) . IS.delete b) a gr) . unUGr
coalesceIfNoEdge a b gr
  | hasEdge a b gr = gr
  | otherwise = coalesce a b gr

replaceIS :: Int -> Int -> IntSet -> IntSet
replaceIS a b as
  | IS.member a as = IS.insert b . IS.delete a $ as
  | otherwise = as

hasEdge :: Node -> Int -> UGraph -> Bool
hasEdge a b ifm = a Nodes.∈ (ifm ! b)

toAscList :: UGraph -> [(Int, Nodes)]
toAscList = IM.toAscList . fmap Nodes . unUnion . unUGr

deleteNode :: Int -> UGraph -> UGraph
deleteNode k = UGr . fmap (IS.delete k) . Map.delete k . unUGr

deleteNodes :: IntSet -> UGraph -> UGraph
deleteNodes ks = UGr . fmap (flip IS.difference ks) . flip (IS.foldr Map.delete) ks . unUGr

insertEdge :: Node -> Int -> UGraph -> UGraph
insertEdge (Node_ k') k = UGr . go k' k . go k k' . unUGr where
    go i j = Map.adjust (IS.insert j) i

insertEdges :: [(Node, Int)] -> UGraph -> UGraph
insertEdges = flip $ foldr (uncurry insertEdge)

empty :: Int -> UGraph
empty k = (UGr . Map.fromList) [(k, IS.empty) | k <- [0..k-1]]

edges :: UGraph -> [(Int, Node)]
edges = toAscList >=> \ (k, ks') -> [(k, k') | k' <- Nodes.toList ks']
