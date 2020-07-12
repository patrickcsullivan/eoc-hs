module CIR.VarUncoverer
  ( uncoverVars
  )
where

import           CIR.AST

uncoverFromStmt :: Stmt -> [Var]
uncoverFromStmt (StmtAssign var _) = [var]

uncoverVars :: Tail -> [Var]
uncoverVars (TailSeq stmt tail) = uncoverFromStmt stmt ++ uncoverVars tail
uncoverVars _                   = []
