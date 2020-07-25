module RIR.Ty where

import qualified Data.Map                      as M
import           RIR.AST

data Ty
  = TyBool
  | TyInt
  deriving Eq

instance Show Ty where
  show ty = case ty of
    TyBool -> "Bool"
    TyInt  -> "Int"

{- | Typing context that maps variable names to types.
-}
type Ctx = M.Map Var Ty

{- | Check the type of the term.
-}
typeOf :: Ctx -> Term -> Either String Ty
typeOf ctx trm = case trm of
  TermRead    -> return TyInt
  TermInt _   -> return TyInt
  (TermNeg t) -> do
    tyT <- typeOf ctx t
    if tyT == TyInt then return TyInt else fail "neg arg must be Int"
  (TermAdd t1 t2) -> do
    tyT1 <- typeOf ctx t1  -- This could short-circuit if t1 is not TyInt,
    tyT2 <- typeOf ctx t2  -- but it's less readable.
    if tyT1 == TyInt && tyT2 == TyInt
      then return TyInt
      else fail "add args but both be Int"
  (TermVar v) -> case M.lookup v ctx of
    Just ty -> return ty
    Nothing -> fail "undefined variable"
  (TermLet v t1 t2) -> do
    tyT1 <- typeOf ctx t1
    let ctx' = M.insert v tyT1 ctx
    typeOf ctx' t2
