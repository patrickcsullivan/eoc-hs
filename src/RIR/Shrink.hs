module RIR.Shrink
  ( shrink
  )
where

import qualified RIR.AST                       as R
import qualified SRIR.AST                      as S

{- | Shrink the RIR term into an SRIR term.
-}
shrink :: R.Term -> S.Term
shrink trm = case trm of
  R.TermRead        -> S.TermRead
  -- Value terms
  (R.TermBool b   ) -> S.TermBool b
  (R.TermInt  n   ) -> S.TermInt n
  -- Integer comparison operators
  (R.TermEq  t1 t2) -> S.TermEq (shrink t1) (shrink t2)
  (R.TermLT  t1 t2) -> S.TermLT (shrink t1) (shrink t2)
  (R.TermLTE t1 t2) -> S.TermNot (S.TermLT (shrink t2) (shrink t1))
  (R.TermGT  t1 t2) -> S.TermLT (shrink t2) (shrink t1)
  (R.TermGTE t1 t2) -> S.TermNot (S.TermLT (shrink t1) (shrink t2))
  -- Logical operators
  (R.TermAnd t1 t2) -> S.TermIf
    (shrink t1)
    (S.TermIf (shrink t2) (S.TermBool True) (S.TermBool False))
    (S.TermBool False)
  (R.TermOr t1 t2) -> S.TermIf
    (shrink t1)
    (S.TermBool True)
    (S.TermIf (shrink t2) (S.TermBool True) (S.TermBool False))
  (R.TermNot t1      ) -> S.TermNot (shrink t1)
  -- Arithmetic operators
  (R.TermNeg t1      ) -> S.TermNeg (shrink t1)
  (R.TermAdd t1 t2   ) -> S.TermAdd (shrink t1) (shrink t2)
  (R.TermSub t1 t2   ) -> S.TermAdd (shrink t1) (S.TermNeg (shrink t2))
  -- Language constructs
  (R.TermVar v       ) -> S.TermVar (toSVar v)
  (R.TermLet v  t1 t2) -> S.TermLet (toSVar v) (shrink t1) (shrink t2)
  (R.TermIf  t1 t2 t3) -> S.TermIf (shrink t1) (shrink t2) (shrink t3)

{- | Convert the RIR variable into an SRIR variable.
-}
toSVar :: R.Var -> S.Var
toSVar (R.Var name) = S.Var name
