{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module RegAlloc (Operation (..), RegCount, allocRegs, allocRegs', colorize) where

import Prelude hiding (id, (.))
import Control.Applicative
import Control.Category
import Control.Monad (guard, join)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Bool (bool)
import Data.Foldable (find, toList, traverse_)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified Data.List as List
import Data.Ord (Down (..))
import Data.Peano
import Data.Traversable (for)
import qualified Lens.Micro.Mtl as ML
import Lens.Micro.TH (makeLenses)
import Util hiding ((∈))

import RegAlloc.Interference (Interferences, Node (..), Operands, UGraph, interferences, interferes, (!), (∈))
import qualified RegAlloc.Interference as If
import qualified RegAlloc.Nodes as Nodes
import qualified RegAlloc.UGraph as UGr

data St = St
  { _degree :: !Int
  , _ifs :: !Interferences
  , _moves :: !Moves
  }
  deriving (Eq, Show)

type Moves = UGraph

$(makeLenses ''St)

allocRegs :: (Traversable f) => RegCount -> f Operation -> Except Interferences (f Int)
allocRegs deg insns = do
    colors <- (allocRegs' deg ifm >=> uncurry (colorize deg ifm)) moves
    for (count insns) \ (k, _) -> maybe (throwError ifm) pure $ colors IM.!? k
  where
    ifm = interferences insns'
    insns' = (\ case NonMove xs -> xs; Move x -> Nodes.fromList [x]) <$> insns
    moves =
        UGr.insertEdges [(k', k :: Int) | (k, Move k') <- toList (count insns)] (UGr.empty deg)

allocRegs' :: RegCount -> Interferences -> Moves -> Except Interferences ([Op], Colors)
allocRegs' deg ifm theMoves =
    execWriterT . flip evalStateT (St { _degree = 1, _ifs = ifm, _moves = theMoves }) $
    whileM (untilFixpointBy (==) (simplifyAndCoalescePhase >> freezePhase) >>
            not . If.null <$> ML.use ifs) potentialSpillPhase
  where
    simplifyAndCoalescePhase = doWhileM bumpDegree do
        St { _degree = deg } <- get
        untilFixpointBy (==) do
            _ <- untilFixpointBy (==) simplify
            St { _ifs = theIfs, _moves = theMoves } <- get
            let isCoalescibleMove (k, k') =
                    not (interferes k' k theIfs) && coalescible deg k' k theIfs
            for (find isCoalescibleMove (UGr.edges theMoves)) \ (k, k') -> coalesce1 k' k
    freezePhase = do
        St { _degree = deg, _ifs = theIfs, _moves = theMoves } <- get
        case [k | k <- [0..deg-1]
                , k' <- Nodes.toList (theMoves ! k)
                , not $ If.interferes k' k theIfs
                , Nodes.size (theIfs ! k) < deg] of
            [] -> pure ()
            k:_ -> () <$ ML.assign moves (UGr.deleteNode k theMoves)
    potentialSpillPhase = do
        k <- ML.zoom ifs potentialSpill
        deleteNode k
        tell ([Select k], IM.empty)
    bumpDegree = compare deg <$> ML.use degree >>= \ case
        GT -> True <$ ML.modifying degree (+1)
        _  -> pure False

colorize
 :: (MonadError Interferences m, Foldable t)
 => RegCount -> Interferences -> t Op -> Colors -> m Colors
colorize deg ifm = execStateT . traverse_ \ case
    Select k -> do
        colors <- get
        let nbrs = ifm ! k
            nbrColors = IS.fromList
                [c | nbr <- Nodes.toList nbrs
                   , Just c <- [case nbr of
                                    Node k -> colors IM.!? k
                                    Precolored c -> Just c]]
        go k $ find (`IS.notMember` nbrColors) [0..deg-1]
    Coalesce k k' -> go k =<< gets (IM.!? k')
  where go k = maybe (throwError ifm) (modify . IM.insert k)

data Op = Select !Int | Coalesce !Int !Int
  deriving (Show)

deleteNode :: MonadState St m => Int -> m ()
deleteNode k = traverse_ ($ UGr.deleteNode k) [ML.modifying ifs, ML.modifying moves]

deleteNodes :: MonadState St m => IntSet -> m ()
deleteNodes ks = traverse_ ($ UGr.deleteNodes ks) [ML.modifying ifs, ML.modifying moves]

coalesce1 :: (MonadState St m, MonadWriter ([Op], Colors) m) => Node -> Int -> m ()
coalesce1 k' k = do
    traverse_ ($ UGr.coalesce k' k) [ML.modifying ifs, ML.modifying moves]
    tell case k' of
        Node k' -> ([Coalesce k k'], IM.empty)
        Precolored c -> ([], IM.singleton k c)

simplify :: (MonadState St m, MonadWriter ([Op], Colors) m) => m ()
simplify =
    concatMap IS.toList <$> untilFixpointBy (==) simplify1 >>=
    tell . flip (,) IM.empty . fmap Select

simplify1 :: MonadState St m => m IntSet
simplify1 = do
    St { _degree = deg, _ifs = theIfs, _moves = theMoves } <- get
    let moveRelateds = foldMap IS.fromList [[k, k'] | (k, Node k') <- UGr.edges theMoves]
        delenda = IS.fromAscList
            [k | (k, ks) <- If.toAscList theIfs
               , IS.notMember k moveRelateds && k >= 0 && Nodes.size ks < deg]
    delenda <$ deleteNodes delenda

untilFixpointBy :: (MonadState s m) => (s -> s -> Bool) -> m a -> m [a]
untilFixpointBy eq x = go [] where
    go as = do
        s <- get
        a <- x
        t <- get
        bool (go . (a:)) pure (eq s t) as

whileM :: Monad m => m Bool -> m a -> m [a]
whileM = compose2 whileJust (fmap guard) pure

doWhileM :: Monad m => m Bool -> m a -> m [a]
doWhileM p = liftA2 (:) <*> whileM p

coalescible :: RegCount -> Node -> Int -> Interferences -> Bool
coalescible n = join \ case
    Node       _ -> briggs n
    Precolored _ -> george n

briggs, george :: RegCount -> Node -> Int -> Interferences -> Bool
briggs n a b ifm = (fromIntegral n :: Peano) > flip count' (Nodes.toList (ifm' ! b)) \ case
     Node c -> Nodes.size (ifm' ! c) > n
     Precolored _ -> True
  where
    ifm' = UGr.coalesce a b ifm
    count' f = List.genericLength . filter f
george n a b ifm = flip all (Nodes.toList (ifm ! b)) \ case
    Node c -> let nbrs = ifm ! c
              in Nodes.size nbrs < n || a ∈ nbrs
    c@(Precolored _) -> all (c ∈) aNbrs'
  where
    aNbrs' = case a of
        Precolored _ -> Nothing
        Node a -> Just (ifm ! a)

potentialSpill :: (MonadState Interferences m, MonadError Interferences m) => m Int
potentialSpill = List.sortOn (Down . Nodes.size . snd) . If.toAscList <$> get >>= \ case
    [] -> get >>= throwError
    (k, _):_ -> pure k

data Operation = Move !Node | NonMove !Operands

type RegCount = Int
type Colors = IntMap Int
