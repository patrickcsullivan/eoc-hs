module PXIR.PatchInstructions
  ( patchInstructions
  )
where

import           PXIR.AST

{- | Return true iff the argument is a memory reference.
-}
isDRef :: Arg -> Bool
isDRef (ArgDeref _ _) = True
isDRef _              = False

{- | If the given instruction is allowed to have at most one memory reference
argument then patch the instruction so that it adheres to this rule. 

In general if an instruction has a src and a dst argument that are both memory
references we patch the instruction by moving the src into the RAX register. We
then perform the original instruction using RAX as the src and the original dst
argument as the dst.
-}
patchDoubleRef :: Instr -> [Instr]
patchDoubleRef instr = case instr of
  (InstrMovQ src dst) | isDRef src && isDRef dst ->
    [InstrMovQ src (ArgReg RegRAX), InstrMovQ (ArgReg RegRAX) dst]
  (InstrAddQ src dst) | isDRef src && isDRef dst ->
    [InstrMovQ src (ArgReg RegRAX), InstrAddQ (ArgReg RegRAX) dst]
  (InstrSubQ src dst) | isDRef src && isDRef dst ->
    [InstrMovQ src (ArgReg RegRAX), InstrSubQ (ArgReg RegRAX) dst]
  _ -> [instr]

{- | If the given instruction is a movq with matching a source and destination
then remove the instruction.
-}
patchUnneededMovQ :: Instr -> [Instr]
patchUnneededMovQ instr = case instr of
  (InstrMovQ src dst) | src == dst -> []
  _ -> [instr]

{- | Perform various patches on the instructions that either optimize the
instructions or enforce x86 rules so that the returned instructions are valid
x86 assembly.
-}
patchInstructions :: [Instr] -> [Instr]
patchInstructions instrs = do
  i  <- instrs
  i' <- patchUnneededMovQ i
  patchDoubleRef i'
