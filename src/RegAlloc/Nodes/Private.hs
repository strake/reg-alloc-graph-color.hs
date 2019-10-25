module RegAlloc.Nodes.Private where

import Data.IntSet (IntSet)

newtype Node = Node_ { unNode_ :: Int }
  deriving (Eq, Show)

newtype Nodes = Nodes { unNodes :: IntSet }
  deriving (Eq, Show)
