module SRIR.SimplifyArgs
  ( simplifyArgs
  )
where

import           Control.Monad.State
import qualified Data.Map                      as M
import           SRIR.AST

{- | Context that maintains the state necessary for simplifying operation
arguments in the AST.
-}
data Ctx = Ctx
    {- | The next number that will be used when generating a unique variable
    name.
    -}
    { ctxNextVar :: !Int
    }

{- | Create a new context where the next generated variable name will contain
the given value.
-}
newCtx :: Int -> Ctx
newCtx nextVar = Ctx { ctxNextVar = nextVar }

{- | State monad that wraps the context.
-}
type CtxS a = State Ctx a

{-| Generate a unique variable. Increment the context's coounter. Return the
generated variable. 
-}
genVar :: CtxS Var
genVar = do
  ctx <- get
  let nextVar = ctxNextVar ctx
  let gendVar = Var ("_" ++ show nextVar)
  put $ ctx { ctxNextVar = nextVar + 1 }
  return gendVar

{-| Returns true iff the term is a complex arg.
-}
isComplex :: Term -> Bool
isComplex (TermInt _) = False
isComplex (TermVar _) = False
isComplex _           = True

{-| Add new variable bindings into the term so that every argument to an
operation is either a value term or a variable term.
-}
simplify :: Term -> CtxS Term
simplify trm = case trm of
  TermRead              -> return TermRead
  (TermBool b         ) -> return (TermBool b)
  (TermInt  n         ) -> return (TermInt n)
  (TermEq arg1 arg2   ) -> simplifyOperator2 TermEq arg1 arg2
  (TermLT arg1 arg2   ) -> simplifyOperator2 TermLT arg1 arg2
  (TermNot arg        ) -> simplifyOperator1 TermNot arg
  (TermNeg arg        ) -> simplifyOperator1 TermNeg arg
  (TermAdd arg1 arg2  ) -> simplifyOperator2 TermAdd arg1 arg2
  (TermVar var        ) -> return (TermVar var)
  (TermLet var bnd bdy) -> do
    bnd' <- simplify bnd
    bdy' <- simplify bdy
    return (TermLet var bnd' bdy')
  (TermIf t1 t2 t3) -> do
    t1' <- simplify t1
    t2' <- simplify t2
    t3' <- simplify t3
    return (TermIf t1' t2' t3')

simplifyOperator1 :: (Term -> Term) -> Term -> CtxS Term
simplifyOperator1 mkOperator arg = if isComplex arg
  then do
    gendVar <- genVar
    arg'    <- simplify arg
    return (TermLet gendVar arg' (mkOperator (TermVar gendVar)))
  else return (mkOperator arg)

simplifyOperator2 :: (Term -> Term -> Term) -> Term -> Term -> CtxS Term
simplifyOperator2 mkOperator arg1 arg2 =
  case (isComplex arg1, isComplex arg2) of
    (True, True) -> do
      gendVarX <- genVar
      arg1'    <- simplify arg1
      gendVarY <- genVar
      arg2'    <- simplify arg2
      return
        (TermLet
          gendVarX
          arg1'
          (TermLet gendVarY
                   arg2'
                   (mkOperator (TermVar gendVarX) (TermVar gendVarY))
          )
        )
    (True, False) -> do
      gendVarX <- genVar
      arg1'    <- simplify arg1
      return (TermLet gendVarX arg1' (mkOperator (TermVar gendVarX) arg2))
    (False, True) -> do
      gendVarY <- genVar
      arg2'    <- simplify arg2
      return (TermLet gendVarY arg2' (mkOperator arg1 (TermVar gendVarY)))
    (False, False) -> return (mkOperator arg1 arg2)

{-| Add new variable bindings into the term so that every argument to an
operation is either a value term or a variable term. Each generated variable
name contains an incremented integer with values starting at the given value.
Return the transformed term and the next available variable number.
-}
simplifyArgs :: Term -> Int -> (Term, Int)
simplifyArgs trm nextVar =
  let (trm', ctx') = runState (simplify trm) (newCtx nextVar)
  in  (trm', ctxNextVar ctx')
