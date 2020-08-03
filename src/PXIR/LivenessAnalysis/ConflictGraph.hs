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

{- | Return a graph of conflicts between the destination variable and any
live-after variables that aren't equal to the destination.
-}
dstToLiveAfterConflicts :: Arg -> S.Set Var -> ConflictGraph
dstToLiveAfterConflicts (ArgVar dstVar) liveAfter =
  let liveAfterVerts = map Left $ filter (/= dstVar) $ S.toList liveAfter
  -- Add dst to graph regardles of it conflicts with anything.
  -- Connect the dst var with every live-after value that isn't the dst.
  in  G.vertex (Left dstVar)
        `G.overlay` bidirectionalBiclique [Left dstVar] liveAfterVerts
dstToLiveAfterConflicts _ _ = G.empty

{- | Return a graph of conflicts between the destination variable and any
live-after variables aren't equal to the source or the destination.
-}
dstToLiveAfterExceptSrcConflicts :: Arg -> Arg -> S.Set Var -> ConflictGraph
dstToLiveAfterExceptSrcConflicts srcArg (ArgVar dstVar) liveAfter =
  let liveAfterVerts =
          map Left $ filter (\v -> v /= dstVar && ArgVar v /= srcArg) $ S.toList
            liveAfter
  -- Add dst to graph regardles of it conflicts with anything.
  -- Connect the dst var with every live-after var that isn't the src or dst.
  in  G.vertex (Left dstVar)
        `G.overlay` bidirectionalBiclique [Left dstVar] liveAfterVerts
dstToLiveAfterExceptSrcConflicts _ _ _ = G.empty

{- | Return a graph of conflicts for a callq instruction.
-}
callqConflicts :: S.Set Var -> ConflictGraph
callqConflicts liveAfter =
  let callerSavedRegVerts = map Right callerSavedRegs
      liveAfterVerts      = map Left $ S.toList liveAfter
  in  bidirectionalBiclique callerSavedRegVerts liveAfterVerts

{- | Return a graph of conflicts for an instruction.
-}
instrConflicts :: (Instr, S.Set Var) -> ConflictGraph
instrConflicts (instr, liveAfter) = case instr of
  InstrAddQ _ dst     -> dstToLiveAfterConflicts dst liveAfter
  InstrSubQ _ dst     -> dstToLiveAfterConflicts dst liveAfter
  InstrNegQ dst       -> dstToLiveAfterConflicts dst liveAfter
  InstrXOrQ   _   dst -> dstToLiveAfterConflicts dst liveAfter
  InstrCmpQ   _   _   -> G.empty
  InstrMovQ   src dst -> dstToLiveAfterExceptSrcConflicts src dst liveAfter
  -- Maybe InstrMovZBQ should use dstToLiveAfterExceptSrcConflicts if the bsrc
  -- field is ever changed from a ByteReg to an Arg.
  InstrMovZBQ _   dst -> dstToLiveAfterConflicts dst liveAfter
  InstrPushQ _        -> G.empty
  InstrPopQ  dst      -> dstToLiveAfterConflicts dst liveAfter
  InstrCallQ _        -> callqConflicts liveAfter
  InstrRetQ           -> G.empty
  InstrJmp _          -> G.empty
  InstrJmpIf _ _      -> G.empty
  InstrLabel _        -> G.empty

{- | Build a conflict graph for the instructions.
-}
make :: [(Instr, S.Set Var)] -> ConflictGraph
make = foldl
  (\gAccum instrLiveAfter -> G.overlay gAccum $ instrConflicts instrLiveAfter)
  G.empty
