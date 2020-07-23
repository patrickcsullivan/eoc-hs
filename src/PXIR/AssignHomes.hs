module PXIR.AssignHomes
  ( assignHomesInBlock
  )
where

import           Control.Monad.State
import qualified Data.Map                      as M
import           PXIR.AST
import           PXIR.LivenessAnalysis.ColorGraph
                                                ( colorGraph )
import qualified PXIR.LivenessAnalysis.ConflictGraph
                                               as CG
import           PXIR.LivenessAnalysis.UncoverLive
                                                ( liveAfterEach )

{- | Context that maintains the state necessary for assigning variables to
locations on the stack.
-}
data Ctx = Ctx
    {- | Maps symbols to its storage location in the stack frame. Storage
    location is represented as an offset in bytes from the base pointer.
    -}
    { ctxVarToOffset :: !(M.Map Var Int)
    }

{- | Create a new context with an empty variable-to-offset mapping.
-}
newCtx :: Ctx
newCtx = Ctx { ctxVarToOffset = M.empty }

{- | State monad that wraps the context.
-}
type CtxS a = State Ctx a

{- | Get the space needed for stack variables in bytes.
-}
stackSpace :: Ctx -> Int
stackSpace ctx = 8 * (M.size $ ctxVarToOffset ctx)

{- | Insert a new variable-to-offset mapping.
-}
insertVarOffset :: Var -> Int -> CtxS ()
insertVarOffset var off = do
  ctx <- get
  let ctx' = Ctx $ M.insert var off (ctxVarToOffset ctx)
  put ctx'
  return ()

{- | Return the dereference argument for the given variable. If the variable is
not already saved in the state's variable-to-offset mapping then determine the
variable's offset and save the new variable-to-offset mapping.
-}
getHome :: Var -> CtxS Arg
getHome var = do
  ctx <- get
  case M.lookup var (ctxVarToOffset ctx) of
    (Just off) -> return $ ArgDeref RegRBP off
    Nothing    -> do
      let off = -1 * (stackSpace ctx + 8)
      insertVarOffset var off
      return $ ArgDeref RegRBP off

{- | Replace any variable argument with a dereference argument to the correct
memory location.
-}
assignHomesInArg :: Arg -> CtxS Arg
assignHomesInArg arg = case arg of
  (ArgVar var) -> getHome var
  _            -> return arg

{- | Replace each variable argument in the instruction with a dereference
argument to the correct memory location.
-}
assignHomesInInstr :: Instr -> CtxS Instr
assignHomesInInstr instr = case instr of
  (InstrAddq src dst) -> do
    src' <- assignHomesInArg src
    dst' <- assignHomesInArg dst
    return $ InstrAddq src' dst'
  (InstrSubq src dst) -> do
    src' <- assignHomesInArg src
    dst' <- assignHomesInArg dst
    return $ InstrSubq src' dst'
  (InstrMovq src dst) -> do
    src' <- assignHomesInArg src
    dst' <- assignHomesInArg dst
    return $ InstrMovq src' dst'
  (InstrNegq dst) -> do
    dst' <- assignHomesInArg dst
    return $ InstrNegq dst'
  _ -> return instr

{- | Replace each variable argument in the block with a dereference argument to
the correct memory location. Return the space needed for stack variables in
bytes
-}
assignHomesInBlock :: Block -> (Block, Int)
assignHomesInBlock (Block label instrs) =
  let (instrs', ctx') = runState assignInInstrs newCtx
  in  (Block label instrs', stackSpace ctx')
  where assignInInstrs = mapM assignHomesInInstr instrs

----------------------------------------------------------

-- unocoverLive -> conflictGraph -> colorGraph

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

assignHomes' :: [Reg] -> [Instr] -> ([Instr], Int)
assignHomes' availRegs instrs =
  let varToArg = mapColorToArg availRegs $ assignColors instrs
  in  map (replaceVarsInInstr varToArg) instrs
