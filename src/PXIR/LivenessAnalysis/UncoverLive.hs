module PXIR.LivenessAnalysis.UncoverLive
  ( liveAfterEach
  )
where

import qualified Data.Maybe                    as M
import qualified Data.Set                      as S
import           PXIR.AST

argFromVar :: Arg -> Maybe Var
argFromVar (ArgVar var) = Just var
argFromVar _            = Nothing

argsRead :: Instr -> [Arg]
argsRead instr = case instr of
  InstrAddq src dst -> [src, dst]
  InstrSubq src dst -> [src, dst]
  InstrMovq src _   -> [src]
  InstrNegq  dst    -> [dst]
  InstrPushq src    -> [src]
  InstrPopq  _      -> []
  InstrCallq _      -> []
  InstrJumpq _      -> []
  InstrRetq         -> []

argsWritten :: Instr -> [Arg]
argsWritten instr = case instr of
  InstrAddq _ dst -> [dst]
  InstrSubq _ dst -> [dst]
  InstrMovq _ dst -> [dst]
  InstrNegq  dst  -> [dst]
  InstrPushq _    -> []
  InstrPopq  dst  -> [dst]
  InstrCallq _    -> []
  InstrJumpq _    -> []
  InstrRetq       -> []

varsRead :: Instr -> S.Set Var
varsRead instr = S.fromList $ M.mapMaybe argFromVar $ argsRead instr

varsWritten :: Instr -> S.Set Var
varsWritten instr = S.fromList $ M.mapMaybe argFromVar $ argsWritten instr

liveBefore :: Instr -> S.Set Var -> S.Set Var
liveBefore instr liveAfter =
  (liveAfter `S.difference` varsWritten instr) `S.union` varsRead instr

liveAfterEach :: [Instr] -> [(Instr, S.Set Var)]
liveAfterEach instrs = foldl f [] $ reverse instrs
 where
  -- f calculates the live after vars of the instruction at position n
  f accum instr = case accum of
    -- The final instruction has no live-after vars.
    [] -> [(instr, S.empty)]
    -- Calculate the live-before vars of the instruction at position
    -- n+1. The live-before vars for instruction n+1 will be the
    -- live-after vars for instruction n.
    (succInstr, succLiveAfter) : tail ->
      let succLiveBefore = liveBefore succInstr succLiveAfter
      in  (instr, succLiveBefore) : (succInstr, succLiveAfter) : tail
