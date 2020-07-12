module RIR.ControlExplicator
  ( explicateControl
  )
where

import qualified CIR.AST                       as C
import qualified Data.Map                      as M
import qualified RIR.AST                       as R

{-| Convert an RIR operator argument to a CIR term. Only works for RIR val terms
and var terms since the uniquify args pass should have converted all operator
args into val or var terms. 
-}
rArgToC :: R.Term -> C.Arg
rArgToC (R.TermVal (R.ValueInt i   )) = C.ArgInt i
rArgToC (R.TermVar (R.Var      name)) = C.ArgVar (C.Var name)
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

explicateLetBody :: R.Term -> Maybe (C.Var, C.Tail) -> C.Tail
explicateLetBody rTrm maybeVarAndTail = case rTrm of
  R.TermRead ->
    let cTrm = C.TermRead in prependTermToTail cTrm maybeVarAndTail
  R.TermVal (R.ValueInt i) ->
    let cTrm = C.TermArg (C.ArgInt i) in prependTermToTail cTrm maybeVarAndTail
  R.TermNeg op ->
    let cTrm = C.TermNeg (rArgToC op) in prependTermToTail cTrm maybeVarAndTail
  R.TermAdd opX opY ->
    let cTrm = C.TermAdd (rArgToC opX) (rArgToC opY)
    in  prependTermToTail cTrm maybeVarAndTail
  R.TermVar (R.Var name) ->
    let cTrm = C.TermArg (C.ArgVar (C.Var name))
    in  prependTermToTail cTrm maybeVarAndTail
  R.TermLet (R.Var name) bnd bdy ->
    let tail = explicateLetBody bdy maybeVarAndTail
    in  explicateLetBinding bnd (C.Var name) tail

explicateLetBinding :: R.Term -> C.Var -> C.Tail -> C.Tail
explicateLetBinding binding assignTo tail = case binding of
  R.TermRead ->
    let assigned = C.TermRead
    in  C.TailSeq (C.StmtAssign assignTo assigned) tail
  R.TermVal (R.ValueInt i) ->
    let assigned = C.TermArg (C.ArgInt i)
    in  C.TailSeq (C.StmtAssign assignTo assigned) tail
  R.TermNeg op ->
    let assigned = C.TermNeg (rArgToC op)
    in  C.TailSeq (C.StmtAssign assignTo assigned) tail
  R.TermAdd opX opY ->
    let assigned = C.TermAdd (rArgToC opX) (rArgToC opY)
    in  C.TailSeq (C.StmtAssign assignTo assigned) tail
  R.TermVar (R.Var name) ->
    let assigned = C.TermArg (C.ArgVar (C.Var name))
    in  C.TailSeq (C.StmtAssign assignTo assigned) tail
  R.TermLet (R.Var name) bnd bdy ->
    let tailWithParentAssn = explicateLetBody bdy $ Just (assignTo, tail)
    in  explicateLetBinding bnd (C.Var name) tailWithParentAssn

{-| Make the execution order of the RIR term explicit by converting it into a
CIR tail.
-}
explicateControl :: R.Term -> C.Tail
explicateControl trm = case trm of
  R.TermRead                 -> C.TailRet C.TermRead
  (R.TermVal (R.ValueInt i)) -> C.TailRet (C.TermArg (C.ArgInt i))
  (R.TermNeg op            ) -> C.TailRet (C.TermNeg (rArgToC op))
  (R.TermAdd opX opY) -> C.TailRet (C.TermAdd (rArgToC opX) (rArgToC opY))
  (R.TermVar (R.Var name)  ) -> C.TailRet (C.TermArg (C.ArgVar (C.Var name)))
  (R.TermLet (R.Var name) bnd bdy) ->
    let tail = explicateLetBody bdy Nothing
    in  explicateLetBinding bnd (C.Var name) tail
