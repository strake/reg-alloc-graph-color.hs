module RegAlloc.UGraph
  ( Node (Precolored, Node)
  , UGraph
  , Nodes
  , nbrsOf
  , coalesce
  , coalesceIfNoEdge
  , (!), (Nodes.∈)
  , hasEdge
  , toAscList
  , deleteNode
  , deleteNodes
  , nullEdges
  , insertEdge
  , insertEdges
  , empty
  , null
  , edges
  ) where

import Prelude hiding (null)
import RegAlloc.Nodes as Nodes
import RegAlloc.UGraph.Private
