module SRIR.ExplicateControl
  ( explicateControl
  )
where

import qualified CIR.AST                       as C
import qualified Data.Map                      as M
import qualified SRIR.AST                      as S

{-| Convert an SRIR.operator argument to a CIR term. Only works for SRIR.val terms
and var terms since the uniquify args pass should have converted all operator
args into val or var terms. 
-}
rArgToC :: S.Term -> C.Arg
rArgToC (S.TermInt n           ) = C.ArgInt n
rArgToC (S.TermVar (S.Var name)) = C.ArgVar (C.Var name)
rArgToC _ =
  error
    $ "Uniquify args pass should have converted all operator args into val or var terms"

{-| If a tail and variable are given then return a sequence that assigns the
term to the variable and is followed by the tail. Otherwise return a tail that
simply returns the term.
-}
prependTermToTail :: C.Term -> Maybe (C.Var, C.Tail) -> C.Tail
prependTermToTail trm maybeVarAndTail = case maybeVarAndTail of
  Nothing          -> C.TailRet trm
  Just (var, tail) -> C.TailSeq (C.StmtAssign var trm) tail

explicateLetBody :: S.Term -> Maybe (C.Var, C.Tail) -> C.Tail
explicateLetBody rTrm maybeVarAndTail = case rTrm of
  S.TermRead ->
    let cTrm = C.TermRead in prependTermToTail cTrm maybeVarAndTail
  S.TermInt n ->
    let cTrm = C.TermArg (C.ArgInt n) in prependTermToTail cTrm maybeVarAndTail
  S.TermNeg op ->
    let cTrm = C.TermNeg (rArgToC op) in prependTermToTail cTrm maybeVarAndTail
  S.TermAdd opX opY ->
    let cTrm = C.TermAdd (rArgToC opX) (rArgToC opY)
    in  prependTermToTail cTrm maybeVarAndTail
  S.TermVar (S.Var name) ->
    let cTrm = C.TermArg (C.ArgVar (C.Var name))
    in  prependTermToTail cTrm maybeVarAndTail
  S.TermLet (S.Var name) bnd bdy ->
    let tail = explicateLetBody bdy maybeVarAndTail
    in  explicateLetBinding bnd (C.Var name) tail

explicateLetBinding :: S.Term -> C.Var -> C.Tail -> C.Tail
explicateLetBinding binding assignTo tail = case binding of
  S.TermRead ->
    let assigned = C.TermRead
    in  C.TailSeq (C.StmtAssign assignTo assigned) tail
  S.TermInt n ->
    let assigned = C.TermArg (C.ArgInt n)
    in  C.TailSeq (C.StmtAssign assignTo assigned) tail
  S.TermNeg op ->
    let assigned = C.TermNeg (rArgToC op)
    in  C.TailSeq (C.StmtAssign assignTo assigned) tail
  S.TermAdd opX opY ->
    let assigned = C.TermAdd (rArgToC opX) (rArgToC opY)
    in  C.TailSeq (C.StmtAssign assignTo assigned) tail
  S.TermVar (S.Var name) ->
    let assigned = C.TermArg (C.ArgVar (C.Var name))
    in  C.TailSeq (C.StmtAssign assignTo assigned) tail
  S.TermLet (S.Var name) bnd bdy ->
    let tailWithParentAssn = explicateLetBody bdy $ Just (assignTo, tail)
    in  explicateLetBinding bnd (C.Var name) tailWithParentAssn

{-| Make the execution order of the SRIR.term explicit by converting it into a
CIR tail.
-}
explicateControl :: S.Term -> C.Tail
explicateControl trm = case trm of
  S.TermRead               -> C.TailRet C.TermRead
  S.TermInt n              -> C.TailRet (C.TermArg (C.ArgInt n))
  (S.TermNeg op          ) -> C.TailRet (C.TermNeg (rArgToC op))
  (S.TermAdd opX opY     ) -> C.TailRet (C.TermAdd (rArgToC opX) (rArgToC opY))
  (S.TermVar (S.Var name)) -> C.TailRet (C.TermArg (C.ArgVar (C.Var name)))
  (S.TermLet (S.Var name) bnd bdy) ->
    let tail = explicateLetBody bdy Nothing
    in  explicateLetBinding bnd (C.Var name) tail
