module PXIR.LivenessAnalysis.ConflictGraph
  ( ConflictGraph
  , ConflictVertex
  , make
  , maybeVar
  )
where

import qualified Algebra.Graph                 as G
import qualified Data.Set                      as S
import           PXIR.AST

type ConflictVertex = Either Var Reg

type ConflictGraph = G.Graph ConflictVertex

{- | If the conflict graph vertex contains a variable then return the variable.
-}
maybeVar :: ConflictVertex -> Maybe Var
maybeVar (Left var) = Just var
maybeVar _          = Nothing

{- | List of caller-saved registers.
-}
callerSavedRegs :: [Reg]
callerSavedRegs =
  [RegRAX, RegRDX, RegRCX, RegRSI, RegRDI, RegR8, RegR9, RegR10, RegR11]

{- | Return the bi-directional biclique of the two groups of vertices.
-}
bidirectionalBiclique :: [a] -> [a] -> G.Graph a
bidirectionalBiclique xs ys = G.biclique xs ys `G.overlay` G.biclique ys xs

{- | Retrun a graph of conflicts for an arithmetic instruction.
-}
arithConflicts :: Arg -> S.Set Var -> ConflictGraph
arithConflicts (ArgVar dstVar) liveAfter =
  let liveAfterVerts = map Left $ filter (/= dstVar) $ S.toList liveAfter
  in  G.vertex (Left dstVar)  -- Add dst to graph regardles of it conflicts with anything.
        `G.overlay` bidirectionalBiclique [Left dstVar] liveAfterVerts
arithConflicts _ _ = G.empty

{- | Retrun a graph of conflicts for a movq instruction.
-}
movqConflicts :: Arg -> Arg -> S.Set Var -> ConflictGraph
movqConflicts srcArg (ArgVar dstVar) liveAfter =
  let liveAfterVerts =
          map Left $ filter (\v -> v /= dstVar && ArgVar v /= srcArg) $ S.toList
            liveAfter
  in  G.vertex (Left dstVar)  -- Add dst to graph regardles of it conflicts with anything.
        `G.overlay` bidirectionalBiclique [Left dstVar] liveAfterVerts
movqConflicts _ _ _ = G.empty

{- | Retrun a graph of conflicts for a callq instruction.
-}
callqConflicts :: S.Set Var -> ConflictGraph
callqConflicts liveAfter =
  let callerSavedRegVerts = map Right callerSavedRegs
      liveAfterVerts      = map Left $ S.toList liveAfter
  in  bidirectionalBiclique callerSavedRegVerts liveAfterVerts

{- | Retrun a graph of conflicts for an instruction.
-}
instrConflicts :: (Instr, S.Set Var) -> ConflictGraph
instrConflicts (instr, liveAfter) = case instr of
  InstrAddQ _   dst -> arithConflicts dst liveAfter
  InstrSubQ _   dst -> arithConflicts dst liveAfter
  InstrMovQ src dst -> movqConflicts src dst liveAfter
  InstrNegQ  dst    -> arithConflicts dst liveAfter
  InstrCallQ _      -> callqConflicts liveAfter
  _                 -> G.empty

{- | Build a conflict graph for the instructions.
-}
make :: [(Instr, S.Set Var)] -> ConflictGraph
make = foldl
  (\gAccum instrLiveAfter -> G.overlay gAccum $ instrConflicts instrLiveAfter)
  G.empty
