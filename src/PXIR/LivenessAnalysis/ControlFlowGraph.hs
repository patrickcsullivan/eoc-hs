module PXIR.LivenessAnalysis.ControlFlowGraph
  ( ControlFlowGraph
  , make
  , topoSort
  )
where

import qualified Algebra.Graph                 as G
import           Data.Maybe                     ( mapMaybe )
import qualified Data.Set                      as S
import           PXIR.AST

import           Debug.Trace

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
block in the control flow graph.
-}
blockFlow :: [Label] -> Label -> [Instr] -> ControlFlowGraph
blockFlow otherCFGLabels label instrs =
  let succs =
          filter (\lbl -> elem lbl otherCFGLabels) $ mapMaybe succBlock instrs
  in  G.star label succs

{- | Build a control graph for the blocks of instructions.
-}
make :: [(Label, [Instr])] -> ControlFlowGraph
make blocks = foldl f G.empty blocks
 where
  f gAccum (label, instrs) = G.overlay gAccum $ blockFlow labels label instrs
  labels = map fst blocks

{- | Topologically sort the control flow graph starting with the given label.
-}
topoSort :: Label -> ControlFlowGraph -> [Label]
topoSort start cfg = topoSortK [] [start] cfg

topoSortK :: [Label] -> [Label] -> ControlFlowGraph -> [Label]
topoSortK revSorted noIncoming cfg = case noIncoming of
  [] | G.edgeCount cfg > 0 ->
    error "failed to tologically sort control flow graph"
  [] -> reverse revSorted
  (v : vs) ->
    let revSorted'         = v : revSorted
        -- For each edge e from v to m...
        outgoing           = filter (\(src, _) -> src == v) $ G.edgeList cfg
        -- Remove e from the graph...
        cfg'               = removeEdges outgoing cfg
        -- If m has no remaining incoming edges, insert it into the list of
        -- vertices with no incoming edges.
        dsts               = S.fromList $ map snd outgoing
        dstsWithNoIncoming = S.filter (not . hasIncomingEdges cfg') dsts
        -- And be sure to remove the head from the list of incoming edges.
        noIncoming'        = vs ++ S.toList dstsWithNoIncoming
    in  topoSortK revSorted' noIncoming' cfg'

removeEdges :: (Show a, Eq a) => [(a, a)] -> G.Graph a -> G.Graph a
removeEdges es g = foldl (\accum (src, dst) -> G.removeEdge src dst accum) g es

hasIncomingEdges :: (Eq a, Ord a) => G.Graph a -> a -> Bool
hasIncomingEdges g v = length incoming > 0
  where incoming = filter (\(_, dst) -> dst == v) $ G.edgeList g
