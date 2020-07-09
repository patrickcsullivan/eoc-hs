module RIR.ArgSimplifier
  ( simplifyArgs
  )
where

import           Control.Monad.State
import qualified Data.Map                      as M
import           RIR.AST

{- | Maintains state necessary for simplifying operation arguments in the AST.
-}
data Ctx = Ctx
    {- | The next number that will be used when generating a unique variable
    name.
    -}
    { ctxCounter :: !Int
    }

{- | Create a new context with a counter at the given value.
-}
newCtx :: Int -> Ctx
newCtx counter = Ctx { ctxCounter = counter }

{- | ArgSimplifier's state monad.
-}
type ArgSimplifier a = State Ctx a

{-| Generate a unique variable. Increment the context's coounter. Return the
generated variable. 
-}
genVar :: ArgSimplifier Var
genVar = do
  ctx <- get
  let counter = ctxCounter ctx
  let gendVar = Var ("_" ++ show counter)
  put $ ctx { ctxCounter = counter + 1 }
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
simplify :: Term -> ArgSimplifier Term
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
name contains an incremented integer with values starting at the given counter.
Return the transformed term and the final counter value.
-}
simplifyArgs :: Term -> Int -> (Term, Int)
simplifyArgs trm counter =
  let (trm', ctx') = runState (simplify trm) (newCtx counter)
  in  (trm', ctxCounter ctx')
