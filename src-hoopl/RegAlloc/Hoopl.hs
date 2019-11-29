{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module RegAlloc.Hoopl (module RegAlloc.Hoopl, MoveSpec (..), Node (..)) where

import Compiler.Hoopl hiding ((<*>))
import Compiler.Hoopl.Passes.Live
import Control.Applicative
import Control.Monad ((>=>))
import Control.Monad.Trans.Except (Except, throwE)
import Control.Monad.Trans.Writer (WriterT (..), execWriter)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Map.Class (Union (..))
import qualified Data.Map.Class as Map

import RegAlloc hiding (allocRegs)
import RegAlloc.Interference hiding (interferences)
import RegAlloc.UGraph.Private as UGr

interferences :: ∀ n m . (NodeWithVars n, VarSet n ~ IntSet, Monad m) => BwdPass (WriterT UGraph m) n IntSet
interferences = gatherFacts ((getConst :: ∀ a . Const a n -> a) liveLattice) liveness $ \ ks ->
    UGr . Union $ Map.fromList [(k, ks) | k <- IS.toList ks]

allocRegs
 :: (NodeWithVars m, VarSet m ~ IntSet, LabelsPtr entry)
 => (∀ p i o . Applicative p => (Int -> p Int) -> m i o -> p (n i o))
 -> (m O O -> Maybe MoveSpec) -> RegCount -> MaybeC i entry -> Graph m i C -> Except Interferences (Graph n i C)
allocRegs vars = allocRegs3 (liftA3 (,,) vars vars vars)

allocRegs3
 :: (NodeWithVars m, VarSet m ~ IntSet, LabelsPtr entry)
 => (∀ p . Applicative p => (Int -> p Int) -> (m C O -> p (n C O), m O O -> p (n O O), m O C -> p (n O C)))
 -> (m O O -> Maybe MoveSpec) -> RegCount -> MaybeC i entry -> Graph m i C -> Except Interferences (Graph n i C)
allocRegs3 vars isMove deg entryMayC prog = do
    colors <- (allocRegs' deg ifm >=> uncurry (colorize deg ifm)) moves
    traverseGraph3 (vars $ maybe (throwE ifm) pure . (IM.!?) colors) prog
  where
    ifm = execWriter $ analyzeAndRewriteBwd interferences entryMayC prog Map.empty
    moves = foldGraphNodes3 (pure id, \ case
        insn | Just (MoveSpec k') <- isMove insn -> IS.foldr (UGr.insertEdge k') `flip` varsDefd insn
        _ -> id, pure id) prog (UGr.empty deg)

data MoveSpec = MoveSpec { moveFrom :: Node }
