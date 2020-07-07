module RIR.Uniquifier
  ( uniquify
  )
where

import           Control.Monad.State
import qualified Data.Map                      as M
import           RIR.AST

{- | Maintains state necessary for uniquify-ing the variable names in an AST.
-}
data Ctx = Ctx
    {- | The next number that will be used when generating a unique variable
    name.
    -}
    { ctxCounter :: !Int

    {- | Maps variable names from source code to generated unique variable
    names. Contains only variables that are currently in scope.
    -}
    , ctxVarTab :: !(M.Map Var Var)
    }

{- | Create a new context with a counter at zero and an empty variable table.
-}
newCtx :: Ctx
newCtx = Ctx { ctxCounter = 0, ctxVarTab = M.empty }

{- | Uniquifier's state monad.
-}
type Uniquifier a = State Ctx a

{- | Look up the generated variable for a given source variable in the variable
table.
-}
lookupGendVar :: Var -> Uniquifier (Maybe Var)
lookupGendVar srcVar = do
  ctx <- get
  return $ M.lookup srcVar (ctxVarTab ctx)

{- | Insert a mapping between a source variable and a generated variable.
-}
insertSrcToGendVar :: Var -> Var -> Uniquifier ()
insertSrcToGendVar srcVar gendVar = do
  ctx <- get
  let varTab' = M.insert srcVar gendVar (ctxVarTab ctx)
  put $ ctx { ctxVarTab = varTab' }
  return ()

{-| Delete a mapping for the given source variable.
-}
deleteSrcToGendVar :: Var -> Uniquifier ()
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
genVar :: Var -> Uniquifier Var
genVar srcVar = do
  ctx <- get
  let count'  = ctxCounter ctx + 1
  let gendVar = Var ("_" ++ show count')
  let varTab' = M.insert srcVar gendVar (ctxVarTab ctx)
  put $ ctx { ctxCounter = count', ctxVarTab = varTab' }
  return gendVar

{-| Replace variables the term with with generated unique variables. 
-}
uniquifyTerm :: Term -> Uniquifier Term
uniquifyTerm TermRead = do
  return TermRead

uniquifyTerm (TermVal val) = do
  return (TermVal val)

uniquifyTerm (TermNeg trm) = do
  trm' <- uniquifyTerm trm
  return (TermNeg trm')

uniquifyTerm (TermAdd trmX trmY) = do
  trmX' <- uniquifyTerm trmX
  trmY' <- uniquifyTerm trmY
  return (TermAdd trmX' trmY')

uniquifyTerm (TermVar var) = do
  ctx <- get
  let gendVar = case M.lookup var (ctxVarTab ctx) of
        Just v  -> v
        Nothing -> error $ "Undefined variable " ++ show var
  return (TermVar gendVar)

uniquifyTerm (TermLet var bnd bdy) = do
    -- Uniquify the binding term.
  bnd'               <- uniquifyTerm bnd

  -- If var is already in the table then it must shadow a variable with the same
  -- name. Hold onto the generated variable for the shadowed source variable.
  maybeShadowGendVar <- lookupGendVar var

  -- Generate a new unique variable name for the variable in the let.
  gendVar            <- genVar var

  -- Uniquify the body term with the newly generated variable in the var table.
  bdy'               <- uniquifyTerm bdy

  -- Restore the var table to its state before uniquifying the let term.
  case maybeShadowGendVar of
    -- Put the generated variable for the shadowed source variable back into the
    -- var table.
    Just shadowGendVar -> insertSrcToGendVar var shadowGendVar
    -- Remove the variable from the var table since it will be out of scope for
    -- other parts of the AST.
    Nothing            -> deleteSrcToGendVar var

  return (TermLet gendVar bnd' bdy')

{-| Replace variables the term with with generated unique variables. 
-}
uniquify :: Term -> Term
uniquify trm = let (trm', _) = runState (uniquifyTerm trm) newCtx in trm'

