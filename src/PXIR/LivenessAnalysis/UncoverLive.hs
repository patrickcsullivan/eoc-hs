module PXIR.LivenessAnalysis.UncoverLive
  ( liveAfterEach
  )
where

import qualified Data.Maybe                    as M
import qualified Data.Set                      as S
import           PXIR.AST

{- | Return the variable in the argument if the argument contains a variable.
-}
argFromVar :: Arg -> Maybe Var
argFromVar (ArgVar var) = Just var
argFromVar _            = Nothing

{- | Get the set of variables that are read from by the instruction.
-}
varsRead :: Instr -> S.Set Var
varsRead instr = S.fromList $ M.mapMaybe argFromVar argsRead
 where
  argsRead = case instr of
    InstrAddQ src dst -> [src, dst]
    InstrSubQ src dst -> [src, dst]
    InstrMovQ src _   -> [src]
    InstrNegQ  dst    -> [dst]
    InstrPushQ src    -> [src]
    InstrPopQ  _      -> []
    InstrCallQ _      -> []
    InstrJmp   _      -> []
    InstrRetQ         -> []

{- | Get the set of variables that are written to by the instruction.
-}
varsWritten :: Instr -> S.Set Var
varsWritten instr = S.fromList $ M.mapMaybe argFromVar argsWritten
 where
  argsWritten = case instr of
    InstrAddQ _ dst -> [dst]
    InstrSubQ _ dst -> [dst]
    InstrMovQ _ dst -> [dst]
    InstrNegQ  dst  -> [dst]
    InstrPushQ _    -> []
    InstrPopQ  dst  -> [dst]
    InstrCallQ _    -> []
    InstrJmp   _    -> []
    InstrRetQ       -> []

{- | Get the set of variables that are live before the instruction.
-}
liveBefore :: Instr -> S.Set Var -> S.Set Var
liveBefore instr liveAfter =
  (liveAfter `S.difference` varsWritten instr) `S.union` varsRead instr

{- | Get the set of variables that are live after an instruction for each
instruction in the block.
-}
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
