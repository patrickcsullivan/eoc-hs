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
                                                ( liveAfter )

{- | Assign a color, represented by a natural number, to each variable so that
no variables that are live at the same time have the same color. Try to not use
more colors than necessary.
-}
assignColors :: Label -> M.Map Label [Instr] -> M.Map Var Int
assignColors start blocks = colorGraph $ CG.make $ liveAfter start blocks

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
  (InstrAddQ src dst) ->
    let src' = replaceVarInArg varToArg src
        dst' = replaceVarInArg varToArg dst
    in  InstrAddQ src' dst'
  (InstrSubQ src dst) ->
    let src' = replaceVarInArg varToArg src
        dst' = replaceVarInArg varToArg dst
    in  InstrSubQ src' dst'
  (InstrMovQ src dst) ->
    let src' = replaceVarInArg varToArg src
        dst' = replaceVarInArg varToArg dst
    in  InstrMovQ src' dst'
  (InstrNegQ dst) -> let dst' = replaceVarInArg varToArg dst in InstrNegQ dst'
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

{- | Replace each variable argument in each instruction with a register or a
dereference to a memory location on the stack. Return the space needed to store
variables on the stack in bytes
-}
assignHomes
  :: Label -> [Reg] -> M.Map Label [Instr] -> (M.Map Label [Instr], Int)
assignHomes start availRegs blocks =
  let varToArg = mapColorToArg availRegs $ assignColors start blocks
      blocks'  = M.map (map (replaceVarsInInstr varToArg)) blocks
  in  (blocks', stackSpace varToArg)
