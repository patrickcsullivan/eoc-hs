module PXIR.LivenessAnalysis.UncoverLive
  ( liveAfter
  )
where

import           Control.Monad.State
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                )
import qualified Data.Set                      as S
import           PXIR.AST
import qualified PXIR.LivenessAnalysis.ControlFlowGraph
                                               as CFG

{- | Liveness analysis of a block of instructions.
-}
data BlockLiveness = BlockLiveness
    {- | Variables that are live before the first instruction of the block.
    -}
    { liveBefore :: !(S.Set Var)

    {- | List containing each instruction and the variables that are live after 
    it.
    -}
    , liveAfterInstrs :: !([(Instr, S.Set Var)])
    }

{- | Return the variable in the argument if the argument contains a variable.
-}
argFromVar :: Arg -> Maybe Var
argFromVar (ArgVar var) = Just var
argFromVar _            = Nothing

{- | Get the set of variables that are read from by the instruction.
-}
varsRead :: Instr -> S.Set Var
varsRead instr = S.fromList $ mapMaybe argFromVar argsRead
 where
  argsRead = case instr of
    InstrAddQ src dst     -> [src, dst]
    InstrSubQ src dst     -> [src, dst]
    InstrNegQ dst         -> [dst]
    InstrXOrQ   src  dst  -> [src, dst]
    InstrCmpQ   src2 src1 -> [src1, src2]
    InstrMovQ   src  _    -> [src]
    InstrMovZBQ _    _    -> [] -- Need to update if bsrc is changed to an Arg.
    InstrSet    _    _    -> []
    InstrPushQ src        -> [src]
    InstrPopQ  _          -> []
    InstrCallQ _          -> []
    InstrRetQ             -> []
    InstrJmp _            -> []
    InstrJmpIf _ _        -> []
    InstrLabel _          -> []

{- | Get the set of variables that are written to by the instruction.
-}
varsWritten :: Instr -> S.Set Var
varsWritten instr = S.fromList $ mapMaybe argFromVar argsWritten
 where
  argsWritten = case instr of
    InstrAddQ _ dst   -> [dst]
    InstrSubQ _ dst   -> [dst]
    InstrNegQ dst     -> [dst]
    InstrXOrQ   _ dst -> [dst]
    InstrCmpQ   _ _   -> []
    InstrMovQ   _ dst -> [dst]
    InstrMovZBQ _ dst -> [dst]
    InstrSet    _ _   -> [] -- Need to update if bdst is changed to an Arg.
    InstrPushQ _      -> []
    InstrPopQ  dst    -> [dst]
    InstrCallQ _      -> []
    InstrRetQ         -> []
    InstrJmp _        -> []
    InstrJmpIf _ _    -> []
    InstrLabel _      -> []

{- | Get the set of variables that are live before the instruction.
-}
liveBeforeInstr :: Instr -> S.Set Var -> S.Set Var
liveBeforeInstr instr liveAfter =
  (liveAfter `S.difference` varsWritten instr) `S.union` varsRead instr

{- | Get the sets of variables that are live after each instruction in a block.
-}
liveAfterEachInBlock :: S.Set Var -> [Instr] -> [(Instr, S.Set Var)]
liveAfterEachInBlock liveAfterBlock instrs = foldl f [] $ reverse instrs
 where
  -- f calculates the live after vars of the instruction at position n
  f accum instr = case accum of
    -- The final instruction's live-after vars are the given live-after vars for
    -- the entire block.
    [] -> [(instr, liveAfterBlock)]
    -- Calculate the live-before vars of the instruction at position
    -- n+1. The live-before vars for instruction n+1 will be the
    -- live-after vars for instruction n.
    (succInstr, succLiveAfter) : tail ->
      let succLiveBefore = liveBeforeInstr succInstr succLiveAfter
      in  (instr, succLiveBefore) : (succInstr, succLiveAfter) : tail

{- | Get the sets of variables that are live before the block and live after
each instruction in the block.
-}
blockLiveness :: S.Set Var -> [Instr] -> BlockLiveness
blockLiveness liveAfterBlock instrs =
  let liveAfterInstrs = liveAfterEachInBlock liveAfterBlock instrs
      liveBeforeBlock = case liveAfterInstrs of
        []                             -> liveAfterBlock
        ((fstInstr, liveAfterFst) : _) -> liveBeforeInstr fstInstr liveAfterFst
  in  BlockLiveness liveBeforeBlock liveAfterInstrs


{- | Context that maintains the state necessary performing liveness analysis on
blocks in reverse topological order.
-}
data LivenessCtx = LivenessCtx
    {- | The remaining blocks on which to perform liveness analysis, sorted in
    reverse topological order. 
    -}
    { remaining :: ![(Label, [Instr])]

    {- | Blocks on which liveness analysis has been performed. These happen to
    be in topological order, but that is not intentional or importnat since
    these will be inserted back into a map.
    -}
    , complete :: ![(Label, BlockLiveness)]
    }

{- | State monad that wraps the liveness context.
-}
type LivenessCtxS a = State LivenessCtx a

{- | Peform liveness analysis on remaining blocks.
-}
liveAfterS :: LivenessCtxS (M.Map Label BlockLiveness)
liveAfterS = do
  (LivenessCtx rem comp) <- get
  case rem of
    []                           -> return $ M.fromList comp
    ((nxtLbl, nxtInstrs) : rem') -> do
      -- Conservatively assume that the set of live-after vars for the next
      -- block is the union of live-before var sets for all blocks that are
      -- topological successors to the next block in the control flow graph.
      let liveAfterBlock = S.unions
            $ map (\(_, (BlockLiveness liveBefore _)) -> liveBefore) comp
      let liveness = blockLiveness liveAfterBlock nxtInstrs
      put $ LivenessCtx rem' ((nxtLbl, liveness) : comp)
      liveAfterS

{- | Return the variables that are live after each instruction in each block.
-}
liveAfter :: Label -> M.Map Label [Instr] -> (M.Map Label [(Instr, S.Set Var)])
liveAfter start blocks =
  let topoSortedLabels = CFG.topoSort start $ CFG.make $ M.toList blocks
      topoSortedLabelsAndBlocks = map
        (\lbl -> case M.lookup lbl blocks of
          Just instrs -> (lbl, instrs)
          Nothing     -> error "sorted blocks contains nonexistent label"
        )
        topoSortedLabels
      ctx = LivenessCtx (reverse topoSortedLabelsAndBlocks) []
      (labelToBlockLiveness, _) = runState liveAfterS ctx
  in  M.map (\(BlockLiveness _ lifeAfterInstrs) -> lifeAfterInstrs)
            labelToBlockLiveness

