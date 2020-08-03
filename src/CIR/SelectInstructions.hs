module CIR.SelectInstructions
  ( selectInstructions
  )
where

import qualified CIR.AST                       as C
import qualified Data.Map                      as M
import qualified PXIR.AST                      as P

{- | Create PXIR instructions that read and assign the parsed input to the
destination.
-}
assignReadInstrs :: P.Arg -> [P.Instr]
assignReadInstrs (P.ArgReg P.RegRAX) = [P.InstrCallQ (P.Label "read_int")]
assignReadInstrs dst =
  [P.InstrCallQ (P.Label "read_int"), P.InstrMovQ (P.ArgReg P.RegRAX) dst]

{- | Create PXIR instructions that assign the source argument to the
destination.
-}
assignArgInstrs :: P.Arg -> P.Arg -> [P.Instr]
assignArgInstrs src dst = [P.InstrMovQ src dst]

{- | Create PXIR instructions that negate the given operand and assign the
result to the destination.
-}
assignNegInstrs :: P.Arg -> P.Arg -> [P.Instr]
assignNegInstrs a dst | a == dst  = [P.InstrNegQ dst]
                      | otherwise = [P.InstrMovQ a dst, P.InstrNegQ dst]

{- | Create PXIR instructions that add the given operands and assign the result
to the destination.
-}
assignAddInstrs :: P.Arg -> P.Arg -> P.Arg -> [P.Instr]
assignAddInstrs a1 a2 dst | a1 == dst = [P.InstrAddQ a2 dst]
                          | a2 == dst = [P.InstrAddQ a1 dst]
                          | otherwise = [P.InstrMovQ a1 dst, P.InstrAddQ a2 dst]

{- | Create PXIR instructions that logically negate the given operands and
assign the result to the destination.
-}
assignNotInstrs :: P.Arg -> P.Arg -> [P.Instr]
assignNotInstrs a dst
  | a == dst  = [P.InstrXOrQ (P.ArgInt 1) dst]
  | otherwise = [P.InstrMovQ a dst, P.InstrXOrQ (P.ArgInt 1) dst]

{- | Create PXIR instructions that compares the given operands and assigns the
result of the comparison to the destination.
-}
assignCmpInstrs :: P.CC -> P.Arg -> P.Arg -> P.Arg -> [P.Instr]
assignCmpInstrs cc a1 a2 dst =
  [P.InstrCmpQ a2 a1, P.InstrSet cc P.ByteRegAL, P.InstrMovZBQ P.ByteRegAL dst]

{- | Create PXIR instructions that evaluate the given term and assign the result
to the destination.
-}
evalAndAssignInstrs :: P.Arg -> C.Term -> [P.Instr]
evalAndAssignInstrs dst trm = case trm of
  C.TermRead        -> assignReadInstrs dst
  (C.TermArg a1   ) -> assignArgInstrs (cArgToP a1) dst
  (C.TermNeg a1   ) -> assignNegInstrs (cArgToP a1) dst
  (C.TermAdd a1 a2) -> assignAddInstrs (cArgToP a1) (cArgToP a2) dst
  (C.TermNot a1   ) -> assignNotInstrs (cArgToP a1) dst
  (C.TermCmp cmp a1 a2) ->
    assignCmpInstrs (cCmpToP cmp) (cArgToP a1) (cArgToP a2) dst

{- | Convert the CIR label into a PXIR label.
-}
cLabelToP :: C.Label -> P.Label
cLabelToP (C.Label name) = P.Label name

{- | Convert the CIR variable into a PXIR variable.
-}
cVarToP :: C.Var -> P.Var
cVarToP (C.Var name) = P.Var name

{- | Convert the CIR argument into a PXIR argument.
-}
cArgToP :: C.Arg -> P.Arg
cArgToP arg = case arg of
  (C.ArgBool True ) -> P.ArgInt 1
  (C.ArgBool False) -> P.ArgInt 0
  (C.ArgInt  i    ) -> P.ArgInt i
  (C.ArgVar  var  ) -> P.ArgVar (cVarToP var)

{- | Convert the CIR comparison operator into a PXIR comparison code.
-}
cCmpToP :: C.Cmp -> P.CC
cCmpToP cmp = case cmp of
  C.CmpEq -> P.CCE
  C.CmpLT -> P.CCL

{- | Convert the CIR statement into PXIR instructions.
-}
cStmtToP :: C.Stmt -> [P.Instr]
cStmtToP stmt = case stmt of
  (C.StmtAssign var trm) -> evalAndAssignInstrs dst trm
    where dst = P.ArgVar $ cVarToP var

{- | Convert the CIR if tail into PXIR instructions.
-}
cIfToP :: P.Label -> P.Label -> C.Term -> [P.Instr]
cIfToP thnLbl elsLbl pred = case pred of
  (C.TermCmp cmp a1 a2) ->
    [ P.InstrCmpQ (cArgToP a2) (cArgToP a1)
    , P.InstrJmpIf (cCmpToP cmp) thnLbl
    , P.InstrJmp elsLbl
    ]
  -- EoC seems to assume that the predicate will only ever be a term, but
  -- primitive values, variables, and the not operator are all reasonable
  -- possibilities too. Non Boolean terms should not pass type-checking, but we
  -- will still let the predicate handle integer terms by treating non-zero
  -- values as true.
  (C.TermArg a1) ->
    [ P.InstrCmpQ (P.ArgInt 0) (cArgToP a1) -- The arg might not type-check.
    , P.InstrJmpIf P.CCE elsLbl
    , P.InstrJmp thnLbl
    ]
  (C.TermNot a1) ->
    [ P.InstrCmpQ (P.ArgInt 0) (cArgToP a1)
    , P.InstrJmpIf P.CCE thnLbl
    , P.InstrJmp elsLbl
    ]
  -- None of the following terms should type-check, but handle them anyways.
  C.TermRead ->
    assignReadInstrs (P.ArgReg P.RegRAX)
      ++ [ P.InstrCmpQ (P.ArgInt 0) (P.ArgReg P.RegRAX)
         , P.InstrJmpIf P.CCE elsLbl
         , P.InstrJmp thnLbl
         ]
  (C.TermNeg a1) ->
    assignNegInstrs (cArgToP a1) (P.ArgReg P.RegRAX)
      ++ [ P.InstrCmpQ (P.ArgInt 0) (P.ArgReg P.RegRAX)
         , P.InstrJmpIf P.CCE elsLbl
         , P.InstrJmp thnLbl
         ]
  (C.TermAdd a1 a2) ->
    assignAddInstrs (cArgToP a1) (cArgToP a2) (P.ArgReg P.RegRAX)
      ++ [ P.InstrCmpQ (P.ArgInt 0) (P.ArgReg P.RegRAX)
         , P.InstrJmpIf P.CCE elsLbl
         , P.InstrJmp thnLbl
         ]

{- | Convert the CIR tail into PXIR instructions that returns by jumping to the
given label.
-}
cTailToP :: P.Label -> C.Tail -> [P.Instr]
cTailToP jumpOnRet tail = case tail of
  (C.TailSeq stmt t1) -> (cStmtToP stmt) ++ (cTailToP jumpOnRet t1)
  (C.TailRet t1) ->
    (evalAndAssignInstrs (P.ArgReg P.RegRAX) t1) ++ [P.InstrJmp jumpOnRet]
  (C.TailGoTo lbl) -> [P.InstrJmp (cLabelToP lbl)]
  (C.TailIf t1 thnLbl elsLbl) ->
    cIfToP (cLabelToP thnLbl) (cLabelToP elsLbl) t1

{- | Convert the CIR blocks into PXIR blocks. Any block that returns will return
by jumping to the given "conclusion" label.
-}
selectInstructions :: M.Map C.Label C.Tail -> M.Map P.Label [P.Instr]
selectInstructions =
  (M.mapKeys cLabelToP) . (M.map $ cTailToP (P.Label "conclusion"))
