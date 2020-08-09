{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Tests.RegAlloc where

import Control.Applicative
import Control.Monad ((>=>), guard)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Bool (bool)
import Data.Coerce (coerce)
import Data.Either (isRight)
import Data.Filtrable (nub)
import qualified Data.Foldable.Unicode as Foldable
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import GHC.Generics

import Test.LeanCheck.Generic
import Test.LeanCheck.Utils.Types (Nat (..))
import Test.Tasty (TestTree)
import Test.Tasty.LeanCheck

import RegAlloc
import RegAlloc.Interference as If
import RegAlloc.UGraph as UGr
import RegAlloc.Nodes as Nodes
import RegAlloc.Types.Private

test :: TestTree
test = testProperty "allocRegs" \ Problem { regCount, ifs, moves } -> isRight $
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

newtype Tiers a = Tiers { unTiers :: [[a]] }
  deriving (Foldable, Functor, Traversable)

instance Applicative Tiers where
    pure = Tiers . cons0
    liftA2 = coerce . productWith

instance Alternative Tiers where
    empty = Tiers (pure [])

    (<|>) :: ∀ a . Tiers a -> Tiers a -> Tiers a
    (<|>) = coerce ((\/) :: [[a]] -> _)

instance Monad Tiers where
    (>>=) :: ∀ a b . Tiers a -> (a -> Tiers b) -> Tiers b
    (>>=) = flip (coerce (concatMapT :: _ -> [[a]] -> [[b]]))

mapTiers :: ([[a]] -> [[b]]) -> Tiers a -> Tiers b
mapTiers = coerce

instance Listable Problem where
    tiers = unTiers do
        parms@Parms { regCount, nodeCount } <- Tiers tiers
        flip runReaderT parms $ [Problem {..} | (ifs, moves) <- (,) <$> ugrTiers <*> ugrTiers]

data Parms = Parms
  { regCount, nodeCount :: !Int }
  deriving (Eq, Show, Generic)

instance Listable Parms where
    tiers = genericTiers

ugrTiers :: ReaderT Parms Tiers UGraph
ugrTiers = do
    Parms { nodeCount } <- ask
    let checkUniq as
            | as == nub as = as
            | otherwise = error "bad set"
    flip UGr.insertEdges (UGr.empty nodeCount) . checkUniq <$> (mapReaderT . mapTiers) setsOf edgeTiers

edgeTiers :: ReaderT Parms Tiers (Node, Int)
edgeTiers = do
    Parms { nodeCount } <- ask
    k' <- nodeTiers
    lift $ unNat <$> Tiers tiers >>= \ k -> (k', k) <$ guard (unNode_ k' < k && k < nodeCount)

nodeTiers :: ReaderT Parms Tiers Node
nodeTiers = do
    Parms { regCount, nodeCount } <- ask
    lift $ Node_ <$> rangeTiers (-regCount, nodeCount)

rangeTiers :: (Integral a) => (a, a) -> Tiers a
rangeTiers = Tiers . toTiers . rangeList

rangeList :: (Integral a) => (a, a) -> [a]
rangeList (a, b) = [0..b-1] +| [-1,-2..a]
