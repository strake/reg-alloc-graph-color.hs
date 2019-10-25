{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module RegAlloc.Nodes (Node (Precolored, Node), Nodes, toList, fromList, fromAscList, size, (∈)) where

import Control.Monad (guard)
import Data.Bits (complement)
import qualified Data.IntSet as IS

import RegAlloc.Nodes.Private

pattern Precolored :: Int -> Node
pattern Precolored n <- Node_ (\ n -> complement n <$ guard (n < 0) -> Just n)
  where Precolored n = Node_ (complement n)
pattern Node :: Int -> Node
pattern Node n <- Node_ n@((>= 0) -> True) where Node n = Node_ n
{-# COMPLETE Precolored, Node #-}

toList :: Nodes -> [Node]
toList = fmap Node_ . IS.toList . unNodes

fromList :: [Node] -> Nodes
fromList = Nodes . IS.fromList . fmap unNode_

fromAscList :: [Node] -> Nodes
fromAscList = Nodes . IS.fromAscList . fmap unNode_

size :: Nodes -> Int
size = IS.size . unNodes

(∈) :: Node -> Nodes -> Bool
Node_ n ∈ Nodes ns = IS.member n ns
