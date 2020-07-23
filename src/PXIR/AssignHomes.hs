module PXIR.AssignHomes
  ( assignHomes
  )
where

import           Control.Monad.State
import qualified Data.Map                      as M
import           Data.Maybe                     ( mapMaybe )
import           PXIR.AST
import           PXIR.LivenessAnalysis.ColorGraph
                                                ( colorGraph )
import qualified PXIR.LivenessAnalysis.ConflictGraph
                                               as CG
import           PXIR.LivenessAnalysis.UncoverLive
                                                ( liveAfterEach )

{- | Assign a color, represented by a natural number, to each variable so that
no variables that are live at the same time have the same color. Try to not use
more colors than necessary.
-}
assignColors :: [Instr] -> M.Map Var Int
assignColors = colorGraph . CG.make . liveAfterEach

{- | Map each color, represented by a natural number, to an argument. Starting
from color number zero and working upwards, first assign colors to unique
registers in the given list of available registers. If all available registers
are used then assign remaining colors to memory locations on the stack, moving
progressively further down from the stack base pointer.
-}
mapColorToArg :: [Reg] -> M.Map Var Int -> M.Map Var Arg
mapColorToArg availRegs = M.map
  (\color -> if color >= length availRegs
    then
      let offset = (color - (length availRegs) + 1) * (-8)
      in  ArgDeref RegRBP offset
    else ArgReg $ availRegs !! color
  )

{- | If the argument is a variable then return the new argument to which the
variable has been assigned.

Fail and error if the argument is a variable that doesn't have a new argument
assignment.
-}
replaceVarInArg :: M.Map Var Arg -> Arg -> Arg
replaceVarInArg varToArg (ArgVar var) = case M.lookup var varToArg of
  (Just arg) -> arg
  Nothing    -> error $ "variable has not been assigned to a home"
replaceVarInArg _ arg = arg

{- | Replace each variable argument in the instruction with the new argument to
which the variable has been assigned.

Fail and error if the instruction contains a variable that doesn't have a new
argument assignment.
-}
replaceVarsInInstr :: M.Map Var Arg -> Instr -> Instr
replaceVarsInInstr varToArg instr = case instr of
  (InstrAddq src dst) ->
    let src' = replaceVarInArg varToArg src
        dst' = replaceVarInArg varToArg dst
    in  InstrAddq src' dst'
  (InstrSubq src dst) ->
    let src' = replaceVarInArg varToArg src
        dst' = replaceVarInArg varToArg dst
    in  InstrSubq src' dst'
  (InstrMovq src dst) ->
    let src' = replaceVarInArg varToArg src
        dst' = replaceVarInArg varToArg dst
    in  InstrMovq src' dst'
  (InstrNegq dst) -> let dst' = replaceVarInArg varToArg dst in InstrNegq dst'
  _               -> instr

{- | Caclulate the space on the stack in bytes needed to store the variable.
-}
stackSpace :: M.Map Var Arg -> Int
stackSpace varToArg =
  let offsets =
          mapMaybe
              (\arg -> case arg of
                (ArgDeref _ offset) -> Just offset
                _                   -> Nothing
              )
            $ M.elems varToArg
  in  if null offsets then 0 else (-1) * minimum offsets

{- | Replace each variable argument in the block with a register or a
dereference to a memory location on the stack. Return the space needed to store
variables on the stack in bytes
-}
assignHomes :: [Reg] -> [Instr] -> ([Instr], Int)
assignHomes availRegs instrs =
  let varToArg = mapColorToArg availRegs $ assignColors instrs
      instrs'  = map (replaceVarsInInstr varToArg) instrs
  in  (instrs', stackSpace varToArg)
