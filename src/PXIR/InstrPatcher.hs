module PXIR.InstrPatcher
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
patchInstruction :: Instr -> [Instr]
patchInstruction instr = case instr of
  (InstrMovq src dst) | isDRef src && isDRef dst ->
    [InstrMovq src (ArgReg RegRAX), InstrMovq (ArgReg RegRAX) dst]
  (InstrAddq src dst) | isDRef src && isDRef dst ->
    [InstrMovq src (ArgReg RegRAX), InstrAddq (ArgReg RegRAX) dst]
  (InstrSubq src dst) | isDRef src && isDRef dst ->
    [InstrMovq src (ArgReg RegRAX), InstrSubq (ArgReg RegRAX) dst]
  _ -> [instr]

{- | Patch instructions in the block so that if an instruction in the given
block is allowed to have at most one memory reference argument then any
instructions that violate that rule are replaced with instructions that adhere
to the rule. 
-}
patchInstructions :: Block -> Block
patchInstructions (Block label instrs) = Block label $ do
  instr <- instrs
  patchInstruction instr
