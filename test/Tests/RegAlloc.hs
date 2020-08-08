{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Tests.RegAlloc where

import Control.Monad ((>=>), guard)
import Control.Monad.Logic.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Bool (bool)
import qualified Data.Foldable.Unicode as Foldable
import Data.Function (on)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import GHC.Generics

import Test.SmallCheck.Series hiding ((<~>))
import Test.Tasty (TestTree)
import Test.Tasty.SmallCheck

import RegAlloc
import RegAlloc.Interference as If
import RegAlloc.UGraph as UGr
import RegAlloc.Nodes as Nodes
import RegAlloc.Types.Private

test :: TestTree
test = testProperty "allocRegs" \ Problem { regCount, ifs, moves } ->
    case runExcept $ (allocRegs' regCount ifs >=> uncurry (colorize regCount ifs)) moves of
        Left _ -> Right ""
        Right colors -> "" <$ runExcept do
            bool (throwE $ "bad register: " ++ show colors) (pure ()) $ all (< regCount) colors
            bool (throwE $ "bad domain: " ++ show colors) (pure ()) $ IM.keysSet colors == IS.fromAscList (fst <$> If.toAscList ifs)
            bool (throwE $ "bad answer: " ++ show colors) (pure ()) $ flip all (IM.toList colors) \ (k, color) ->
                let nbrColors = (\ case Node k -> colors IM.! k; Precolored c -> c) <$> Nodes.toList (nbrsOf k ifs)
                in color Foldable.∉ nbrColors

data Problem = Problem
  { regCount, nodeCount :: !Int, ifs :: !Interferences, moves :: !UGraph }
  deriving (Eq, Show, Generic)

instance Monad m => Serial m Problem where
    series = series >>- \ parms@Parms { regCount, nodeCount } -> flip runReaderT parms $
        [Problem {..} | (ifs, moves) <- (,) <$> ugrSeries <~> ugrSeries]

data Parms = Parms
  { regCount, nodeCount :: !Int }
  deriving (Eq, Show, Generic)
deriving instance Monad m => Serial m Parms

ugrSeries :: Monad m => ReaderT Parms (Series m) UGraph
ugrSeries = do
    Parms { nodeCount } <- ask
    flip UGr.insertEdges (UGr.empty nodeCount) <$> mapReaderT (sortedListSBy p) edgeSeries
  where p = (<) `on` \ (Node_ i, j) -> (i, j)

edgeSeries :: Monad m => ReaderT Parms (Series m) (Node, Int)
edgeSeries = do
    Parms { nodeCount } <- ask
    k' <- nodeSeries
    lift $ getNonNegative <$> series >>- \ k ->
           (k', k) <$ guard (unNode_ k' < k && k < nodeCount)

nodeSeries :: Monad m => ReaderT Parms (Series m) Node
nodeSeries = do
    Parms { regCount, nodeCount } <- ask
    lift $ Node_ <$> rangeS (-regCount, nodeCount)

sortedListSBy :: Monad m => (a -> a -> Bool) -> Series m a -> Series m [a]
sortedListSBy p as = decDepth (pure []) `interleave` decDepth (as >>- go) where
    go a = (:) a <$> decDepth (pure [] `interleave` do b <- as; guard (p a b); go b)

rangeS :: Monad m => (Int, Int) -> Series m Int
rangeS (a, b) = do
    k <- series
    k <$ guard (k >= a && k < b)

infixl 4 <~>
(<~>) :: MonadLogic m => m (a -> b) -> m a -> m b
a <~> b = a >>- (<$> b)
