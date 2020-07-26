module CIR.SelectInstructions
  ( selectInstructions
  )
where

import qualified CIR.AST                       as C
import qualified PXIR.AST                      as P

{- | Create PXIR instructions that read and assign the parsed input to the
destination.
-}
readInstrs :: P.Arg -> [P.Instr]
readInstrs dst =
  [P.InstrCallQ (P.Label "read_int"), P.InstrMovQ (P.ArgReg P.RegRAX) dst]

{- | Create PXIR instructions that assign the source argument to the
destination.
-}
movqInstrs :: P.Arg -> P.Arg -> [P.Instr]
movqInstrs src dst = [P.InstrMovQ src dst]

{- | Create PXIR instructions that negate the given operand and assign the
result to the destination.
-}
negqInstrs :: P.Arg -> P.Arg -> [P.Instr]
negqInstrs arg dst = if arg == dst
  then [P.InstrNegQ dst]
  else [P.InstrMovQ arg dst, P.InstrNegQ dst]

{- | Create PXIR instructions that add the given operands and assign the result
to the destination.
-}
addqInstrs :: P.Arg -> P.Arg -> P.Arg -> [P.Instr]
addqInstrs argX argY dst
  | argX == dst = [P.InstrAddQ argY dst]
  | argY == dst = [P.InstrAddQ argX dst]
  | otherwise   = [P.InstrMovQ argX dst, P.InstrAddQ argY dst]

{- | Create PXIR instructions that evaluate the given term and assign the result
to the destination.
-}
evalAndAssignInstrs :: C.Term -> P.Arg -> [P.Instr]
evalAndAssignInstrs trm dst = case trm of
  C.TermRead            -> readInstrs dst
  (C.TermArg arg      ) -> movqInstrs (cArgToP arg) dst
  (C.TermNeg arg      ) -> negqInstrs (cArgToP arg) dst
  (C.TermAdd argX argY) -> addqInstrs (cArgToP argX) (cArgToP argY) dst

{-| Convert the CIR variable into a PXIR variable.
-}
cVarToP :: C.Var -> P.Var
cVarToP (C.Var name) = P.Var name

{-| Convert the CIR argument into a PXIR argument.
-}
cArgToP :: C.Arg -> P.Arg
cArgToP (C.ArgInt i  ) = P.ArgInt i
cArgToP (C.ArgVar var) = P.ArgVar (cVarToP var)

{-| Convert the CIR statement into PXIR instructions.
-}
cStmtToP :: C.Stmt -> [P.Instr]
cStmtToP (C.StmtAssign var trm) = evalAndAssignInstrs trm dst
  where dst = P.ArgVar $ cVarToP var

{-| Convert the CIR tail into PXIR instructions that return by jumping to the
given label.
-}
cTailToP :: C.Tail -> P.Label -> [P.Instr]
cTailToP tail jumpOnRet = case tail of
  (C.TailSeq stmt tl) -> (cStmtToP stmt) ++ (cTailToP tl jumpOnRet)
  (C.TailRet trm) ->
    (evalAndAssignInstrs trm (P.ArgReg P.RegRAX)) ++ [P.InstrJmp jumpOnRet]

{-| Convert the CIR tail into PXIR instructions that return by jumping to the
"concolusion" label.
-}
selectInstructions :: C.Tail -> [P.Instr]
selectInstructions tail = cTailToP tail $ P.Label "conclusion"
