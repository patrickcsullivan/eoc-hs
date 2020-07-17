module RIR.SimplifyArgs
  ( simplifyArgs
  )
where

import           Control.Monad.State
import qualified Data.Map                      as M
import           RIR.AST

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

{-| Returns true iff the term is a complex operand.
-}
isComplex :: Term -> Bool
isComplex (TermVal _) = False
isComplex (TermVar _) = False
isComplex _           = True

{-| Add new variable bindings into the term so that every argument to an
operation is either a value term or a variable term.
-}
simplify :: Term -> CtxS Term
simplify TermRead      = return TermRead -- No args so just return.

simplify (TermVal val) = return (TermVal val) -- Primitive so no args so just return.

simplify (TermNeg op ) = if isComplex op
  then do
    gendVar <- genVar
    op'     <- simplify op
    return (TermLet gendVar op' (TermNeg (TermVar gendVar)))
  else return (TermNeg op)

simplify (TermAdd opX opY) = case (isComplex opX, isComplex opY) of
  (True, True) -> do
    gendVarX <- genVar
    opX'     <- simplify opX
    gendVarY <- genVar
    opY'     <- simplify opY
    return
      (TermLet
        gendVarX
        opX'
        (TermLet gendVarY opY' (TermAdd (TermVar gendVarX) (TermVar gendVarY)))
      )

  (True, False) -> do
    gendVarX <- genVar
    opX'     <- simplify opX
    return (TermLet gendVarX opX' (TermAdd (TermVar gendVarX) opY))

  (False, True) -> do
    gendVarY <- genVar
    opY'     <- simplify opY
    return (TermLet gendVarY opY' (TermAdd opX (TermVar gendVarY)))

  (False, False) -> return (TermAdd opX opY)

simplify (TermVar var        ) = return (TermVar var)

simplify (TermLet var bnd bdy) = do
  bnd' <- simplify bnd
  bdy' <- simplify bdy
  return (TermLet var bnd' bdy')

{-| Add new variable bindings into the term so that every argument to an
operation is either a value term or a variable term. Each generated variable
name contains an incremented integer with values starting at the given value.
Return the transformed term and the next available variable number.
-}
simplifyArgs :: Term -> Int -> (Term, Int)
simplifyArgs trm nextVar =
  let (trm', ctx') = runState (simplify trm) (newCtx nextVar)
  in  (trm', ctxNextVar ctx')
