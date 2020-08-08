module RegAlloc.Interference (module RegAlloc.UGraph, Interferences, Operands, interferes, interferences) where

import Control.Monad.Trans.State (state, evalState)
import Data.Functor.Reverse (Reverse (..))
import Data.Map.Class (Union (..))
import qualified Data.Map.Class as Map
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Util hiding ((∈))

import RegAlloc.Types.Private (Nodes (..), UGraph (..))
import RegAlloc.UGraph

type Interferences = UGraph

interferes :: Node -> Int -> Interferences -> Bool
interferes = hasEdge

interferences :: (Traversable f) => f Operands -> UGraph
interferences = (.) UGr $
    liveSets & foldMap \ ks -> Union $ Map.fromList [(k, ks) | k <- IS.toList ks]

liveSets :: (Traversable f) => f Operands -> f LiveSet
liveSets = getReverse . flip evalState IS.empty . traverse (go . fmap unNodes) . Reverse . count
  where go (k, ks) = state $ (,) <*> IS.insert k . flip IS.difference ks

type Operands = Nodes
type LiveSet = IntSet
