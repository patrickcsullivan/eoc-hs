module PXIR.LivenessAnalysis.ControlFlowGraph
  ( ControlFlowGraph
  , make
  )
where

import qualified Algebra.Graph                 as G
import           Data.Maybe                     ( mapMaybe )
import           PXIR.AST

type ControlFlowGraph = G.Graph Label

{- | Return the label of the succsessor block that is linked to by the given
instruction. 
-}
succBlock :: Instr -> Maybe Label
succBlock instr = case instr of
  InstrJmp lbl     -> Just lbl
  InstrJmpIf _ lbl -> Just lbl
  _                -> Nothing

{- | Return a graph connecting the given block of instructions to any successor
blocks.
-}
blockFlow :: Label -> [Instr] -> ControlFlowGraph
blockFlow label instrs =
  let succs = mapMaybe succBlock instrs in G.star label succs

{- | Build a conflict graph for the instructions.
-}
make :: [(Label, [Instr])] -> ControlFlowGraph
make = foldl
  (\gAccum (label, instrs) -> G.overlay gAccum $ blockFlow label instrs)
  G.empty
