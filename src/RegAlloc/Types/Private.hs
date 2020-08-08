module RegAlloc.Types.Private where

import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map.Class (Union (..))

newtype Node = Node_ { unNode_ :: Int }
  deriving (Eq, Show)

newtype Nodes = Nodes { unNodes :: IntSet }
  deriving (Eq, Show)

newtype UGraph = UGr { unUGr :: Union IntMap IntSet }
  deriving (Eq, Show)
