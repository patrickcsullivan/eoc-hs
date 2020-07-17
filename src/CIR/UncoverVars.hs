module CIR.UncoverVars
  ( uncoverVars
  )
where

import           CIR.AST
import qualified Data.Set                      as S

{- | Get a list of all variables in the tail.
-}
uncoverVars :: Tail -> S.Set Var
uncoverVars (TailSeq stmt tail) = case stmt of
  (StmtAssign var _) -> S.insert var (uncoverVars tail)
uncoverVars _ = S.empty
