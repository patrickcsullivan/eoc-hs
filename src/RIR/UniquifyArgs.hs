module RIR.UniquifyArgs
  ( uniquifyArgs
  )
where

import           Control.Monad.State
import qualified Data.Map                      as M
import           RIR.AST

{- | Context that maintains the state necessary for uniquify-ing the variable
names in an AST.
-}
data Ctx = Ctx
    {- | The next number that will be used when generating a unique variable
    name.
    -}
    { ctxNextVar :: !Int

    {- | Maps variable names from source code to generated unique variable
    names. Contains only variables that are currently in scope.
    -}
    , ctxVarTab :: !(M.Map Var Var)
    }

{- | Create a new context with an empty variable table and where the next
generated variable name will contain the given value.
-}
newCtx :: Int -> Ctx
newCtx nextVar = Ctx { ctxNextVar = nextVar, ctxVarTab = M.empty }

{- | State monad that wraps the context.
-}
type CtxS a = State Ctx a

{- | Look up the generated variable for a given source variable in the variable
table.
-}
lookupGendVar :: Var -> CtxS (Maybe Var)
lookupGendVar srcVar = do
  ctx <- get
  return $ M.lookup srcVar (ctxVarTab ctx)

{- | Insert a mapping between a source variable and a generated variable.
-}
insertSrcToGendVar :: Var -> Var -> CtxS ()
insertSrcToGendVar srcVar gendVar = do
  ctx <- get
  let varTab' = M.insert srcVar gendVar (ctxVarTab ctx)
  put $ ctx { ctxVarTab = varTab' }
  return ()

{-| Delete a mapping for the given source variable.
-}
deleteSrcToGendVar :: Var -> CtxS ()
deleteSrcToGendVar srcVar = do
  ctx <- get
  let varTab' = M.delete srcVar (ctxVarTab ctx)
  put $ ctx { ctxVarTab = varTab' }
  return ()

{-| Generate a unique variable for the given source variable. Save the
source-to-generated variable mapping in the context, potentially overwriting a
previously saved mapping for the same source variable name. Increment the
context's coounter. Return the generated variable. 
-}
genVar :: Var -> CtxS Var
genVar srcVar = do
  ctx <- get
  let nextVar = ctxNextVar ctx
  let gendVar = Var ("_" ++ show nextVar)
  let varTab' = M.insert srcVar gendVar (ctxVarTab ctx)
  put $ ctx { ctxNextVar = nextVar + 1, ctxVarTab = varTab' }
  return gendVar

{-| Replace variables the term with with generated unique variables. 
-}
uniquify :: Term -> CtxS Term
uniquify TermRead = do
  return TermRead

uniquify (TermInt n) = do
  return (TermInt n)

uniquify (TermNeg trm) = do
  trm' <- uniquify trm
  return (TermNeg trm')

uniquify (TermAdd trmX trmY) = do
  trmX' <- uniquify trmX
  trmY' <- uniquify trmY
  return (TermAdd trmX' trmY')

uniquify (TermVar var) = do
  ctx <- get
  let gendVar = case M.lookup var (ctxVarTab ctx) of
        Just v  -> v
        Nothing -> error $ "Undefined variable " ++ show var
  return (TermVar gendVar)

uniquify (TermLet var bnd bdy) = do
    -- Uniquify the binding term.
  bnd'               <- uniquify bnd

  -- If var is already in the table then it must shadow a variable with the same
  -- name. Hold onto the generated variable for the shadowed source variable.
  maybeShadowGendVar <- lookupGendVar var

  -- Generate a new unique variable name for the variable in the let.
  gendVar            <- genVar var

  -- Uniquify the body term with the newly generated variable in the var table.
  bdy'               <- uniquify bdy

  -- Restore the var table to its state before uniquifying the let term.
  case maybeShadowGendVar of
    -- Put the generated variable for the shadowed source variable back into the
    -- var table.
    Just shadowGendVar -> insertSrcToGendVar var shadowGendVar
    -- Remove the variable from the var table since it will be out of scope for
    -- other parts of the AST.
    Nothing            -> deleteSrcToGendVar var

  return (TermLet gendVar bnd' bdy')

{-| Replace variables the term with with generated unique variables. Each
generated variable name contains an incremented integer with values starting at
the given value. Return the transformed term and the next available variable
number.
-}
uniquifyArgs :: Term -> Int -> (Term, Int)
uniquifyArgs trm nextVar =
  let (trm', ctx') = runState (uniquify trm) (newCtx nextVar)
  in  (trm', ctxNextVar ctx')

