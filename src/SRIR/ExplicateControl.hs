module SRIR.ExplicateControl
  ( explicateControl
  )
where

import           Control.Monad.State
import qualified CIR.AST                       as C
import qualified Data.Map                      as M
import qualified SRIR.AST                      as S

{- | Context that maintains the state necessary for explicating the flow of
control.
-}
data Ctx = Ctx
    {- | The next number that will be used when generating a unique block label.
    -}
    { ctxNextLabel :: !Int
    , ctxBlocks :: !(M.Map C.Label C.Tail)
    }

{- | Create a new context where the next generated block label will contain the
given value.
-}
newCtx :: Int -> Ctx
newCtx nextLabel = Ctx { ctxNextLabel = nextLabel, ctxBlocks = M.empty }

{- | State monad that wraps the context.
-}
type CtxS a = State Ctx a

{- | Get the next available block label and increment the next label counter.
-}
getNextLabel :: CtxS C.Label
getNextLabel = do
  (Ctx next blocks) <- get
  let label = C.Label $ "block" ++ show next
  put (Ctx (next + 1) blocks)
  return label

{- | Insert the label and tail into the mapping of labels to tails.
-}
insertBlock :: C.Label -> C.Tail -> CtxS ()
insertBlock label tail = do
  (Ctx next blocks) <- get
  let blocks' = M.insert label tail blocks
  put (Ctx next blocks')
  return ()

{- | Convert an SRIR operator argument to a CIR term. Only works for SRIR val
terms and var terms since the uniquify args pass should have converted all
operator args into val or var terms. 
-}
rArgToC :: S.Term -> C.Arg
rArgToC (S.TermBool b           ) = C.ArgBool b
rArgToC (S.TermInt  n           ) = C.ArgInt n
rArgToC (S.TermVar  (S.Var name)) = C.ArgVar (C.Var name)
rArgToC _ =
  error
    $ "Uniquify args pass should have converted all operator args into val or var terms"

{- | Explicate the binding term of a let term.
-}
explicateLetBinding
  :: S.Term -- ^ The binding term.
  -> C.Var  -- ^ The variable from the let to which the result of the binding is assigned.
  -> C.Tail -- ^ Tail for the execution of the let's body and anything else that may follow.
  -> CtxS C.Tail
explicateLetBinding binding assignTo tail = case binding of
  S.TermRead ->
    let assigned = C.TermRead
    in  return $ C.TailSeq (C.StmtAssign assignTo assigned) tail
  S.TermBool b ->
    let assigned = C.TermArg (C.ArgBool b)
    in  return $ C.TailSeq (C.StmtAssign assignTo assigned) tail
  S.TermInt n ->
    let assigned = C.TermArg (C.ArgInt n)
    in  return $ C.TailSeq (C.StmtAssign assignTo assigned) tail
  S.TermEq t1 t2 ->
    let assigned = C.TermCmp C.CmpEq (rArgToC t1) (rArgToC t2)
    in  return $ C.TailSeq (C.StmtAssign assignTo assigned) tail
  S.TermLT t1 t2 ->
    let assigned = C.TermCmp C.CmpLT (rArgToC t1) (rArgToC t2)
    in  return $ C.TailSeq (C.StmtAssign assignTo assigned) tail
  S.TermNot t1 ->
    let assigned = C.TermNot (rArgToC t1)
    in  return $ C.TailSeq (C.StmtAssign assignTo assigned) tail
  S.TermNeg t1 ->
    let assigned = C.TermNeg (rArgToC t1)
    in  return $ C.TailSeq (C.StmtAssign assignTo assigned) tail
  S.TermAdd t1 t2 ->
    let assigned = C.TermAdd (rArgToC t1) (rArgToC t2)
    in  return $ C.TailSeq (C.StmtAssign assignTo assigned) tail
  S.TermVar (S.Var name) ->
    let assigned = C.TermArg (C.ArgVar (C.Var name))
    in  return $ C.TailSeq (C.StmtAssign assignTo assigned) tail
  S.TermLet (S.Var name) bnd bdy -> do
    bdyTail <- explicateTail bdy
                             (\t -> C.TailSeq (C.StmtAssign assignTo t) tail)
    explicateLetBinding bnd (C.Var name) bdyTail
  S.TermIf pred thn els -> do
    -- The given tail needs a label so both branches can go to it.
    tailLabel <- getNextLabel
    insertBlock tailLabel tail
    -- Create labeled block for each branch. Each branch binds result to
    -- variable and then goes to the given tail.
    let assignThenGoToTail =
          (\t -> C.TailSeq (C.StmtAssign assignTo t) (C.TailGoTo tailLabel))
    thnLabel <- getNextLabel
    elsLabel <- getNextLabel
    thnTail  <- explicateTail thn assignThenGoToTail
    elsTail  <- explicateTail els assignThenGoToTail
    insertBlock thnLabel thnTail
    insertBlock elsLabel elsTail
    explicateIfPred pred thnLabel elsLabel

{- | Explicate the predicate term of an if term.
-}
explicateIfPred
  :: S.Term  -- ^ The predicate term.
  -> C.Label -- ^ Label for block that executes "then" term from the enclosing "if".
  -> C.Label -- ^ Label for block that executes "else" term from the enclosing "if".
  -> CtxS C.Tail
explicateIfPred pred encThnLabel encElsLabel = case pred of
  S.TermRead -> return $ C.TailIf C.TermRead encThnLabel encElsLabel -- mistyped
  S.TermBool b ->
    return $ C.TailIf (C.TermArg (C.ArgBool b)) encThnLabel encElsLabel
  S.TermInt n ->
    return $ C.TailIf (C.TermArg (C.ArgInt n)) encThnLabel encElsLabel -- mistyped
  S.TermEq t1 t2 -> return $ C.TailIf
    (C.TermCmp C.CmpEq (rArgToC t1) (rArgToC t2))
    encThnLabel
    encElsLabel
  S.TermLT t1 t2 -> return $ C.TailIf
    (C.TermCmp C.CmpLT (rArgToC t1) (rArgToC t2))
    encThnLabel
    encElsLabel
  S.TermNot t1 ->
    return $ C.TailIf (C.TermNot (rArgToC t1)) encThnLabel encElsLabel
  S.TermNeg t1 ->
    return $ C.TailIf (C.TermNeg (rArgToC t1)) encThnLabel encElsLabel -- mistyped
  S.TermAdd t1 t2 -> return
    $ C.TailIf (C.TermAdd (rArgToC t1) (rArgToC t2)) encThnLabel encElsLabel -- mistyped
  S.TermVar (S.Var name) -> return
    $ C.TailIf (C.TermArg (C.ArgVar (C.Var name))) encThnLabel encElsLabel
  S.TermLet (S.Var name) bnd bdy -> do
    bdyTail <- explicateTail bdy (\t -> C.TailIf t encThnLabel encElsLabel)
    explicateLetBinding bnd (C.Var name) bdyTail
  S.TermIf pred thn els -> do
    -- Create labeled block for each branch. Each branch will evaluate and then
    -- jump to one of the enclosing "if" term's branches.
    thnLabel <- getNextLabel
    elsLabel <- getNextLabel
    thnTail  <- explicateTail thn (\t -> C.TailIf t encThnLabel encElsLabel)
    elsTail  <- explicateTail els (\t -> C.TailIf t encThnLabel encElsLabel)
    insertBlock thnLabel thnTail
    insertBlock elsLabel elsTail
    -- Return a tail will evaluate the inner "if" term's predicate and then jump
    -- to one of the inner "if" term's branches, both of which will evaluate and
    -- then jump to one of the enclosing "if" term's branches.
    explicateIfPred pred thnLabel elsLabel

{- | Explicate the "tail" part of an SRIR term. The "tail" of an SRIR term is
the part of the term that should be executed last.
-}
explicateTail
  :: S.Term             -- ^ Term to explicate.
  -> (C.Term -> C.Tail) -- ^ Produces the tail to execute after evaluating the given term.
  -> CtxS C.Tail
explicateTail rTrm doAfter = case rTrm of
  S.TermRead   -> return $ doAfter C.TermRead
  S.TermBool b -> return $ doAfter $ C.TermArg (C.ArgBool b)
  S.TermInt  n -> return $ doAfter $ C.TermArg (C.ArgInt n)
  S.TermEq t1 t2 ->
    return $ doAfter $ C.TermCmp C.CmpEq (rArgToC t1) (rArgToC t2)
  S.TermLT t1 t2 ->
    return $ doAfter $ C.TermCmp C.CmpLT (rArgToC t1) (rArgToC t2)
  S.TermNot t1    -> return $ doAfter $ C.TermNot (rArgToC t1)
  S.TermNeg t1    -> return $ doAfter $ C.TermNeg (rArgToC t1)
  S.TermAdd t1 t2 -> return $ doAfter $ C.TermAdd (rArgToC t1) (rArgToC t2)
  S.TermVar (S.Var name) ->
    return $ doAfter $ C.TermArg (C.ArgVar (C.Var name))
  S.TermLet (S.Var name) bnd bdy -> do
    doAfterBinding <- explicateTail bdy doAfter -- Tail to execute after evaluating binding.
    explicateLetBinding bnd (C.Var name) doAfterBinding
  S.TermIf pred thn els -> do
    thnLabel <- getNextLabel
    elsLabel <- getNextLabel
    thnTail  <- explicateTail thn doAfter
    elsTail  <- explicateTail els doAfter
    insertBlock thnLabel thnTail
    insertBlock elsLabel elsTail
    explicateIfPred pred thnLabel elsLabel

{- | Make the flow of control of the SRIR top level term explicit by converting
it into a CIR tail. Then assign the tail to the given label.
-}
explicateTop :: S.Term -> C.Label -> CtxS C.Tail
explicateTop trm label = do
  tail <- explicateTail trm (\t -> C.TailRet t)
  insertBlock label tail
  return tail

{- | Make the flow of control of the SRIR top level term explicit by converting
it into a set of labeled CIR tails (aka blocks).
-}
explicateControl
  :: S.Term  -- ^ Term to explicate
  -> C.Label -- ^ Label for the starting block in the new CIR program
  -> Int     -- ^ Next number to use when generating block names
  -> (M.Map C.Label C.Tail, Int)
explicateControl trm start nextLabel =
  let (_, (Ctx nextLabel' blocks)) =
          runState (explicateTop trm start) (newCtx nextLabel)
  in  (blocks, nextLabel')
