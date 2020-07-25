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
  TermRead        -> return TyInt
  -- Value terms
  TermInt  _      -> return TyInt
  TermBool _      -> return TyBool
  -- Integer comparison operators
  TermEq  t1 t2   -> typeOfCompOp ctx t1 t2
  TermLT  t1 t2   -> typeOfCompOp ctx t1 t2
  TermLTE t1 t2   -> typeOfCompOp ctx t1 t2
  TermGT  t1 t2   -> typeOfCompOp ctx t1 t2
  TermGTE t1 t2   -> typeOfCompOp ctx t1 t2
  -- Logical operators
  TermAnd t1 t2   -> typeOfBinaryLogicalOp ctx t1 t2
  TermOr  t1 t2   -> typeOfBinaryLogicalOp ctx t1 t2
  TermNot t1      -> typeOfUnaryLogicalOp ctx t1
  -- Arithmetic operators
  (TermNeg t1   ) -> typeOfUnaryArithOp ctx t1
  (TermAdd t1 t2) -> typeOfBinaryArithOp ctx t1 t2
  (TermSub t1 t2) -> typeOfBinaryArithOp ctx t1 t2
  -- Language constructs
  (TermVar v    ) -> case M.lookup v ctx of
    Just ty -> return ty
    Nothing -> fail "undefined variable"
  (TermLet v t1 t2) -> do
    tyT1 <- typeOf ctx t1
    let ctx' = M.insert v tyT1 ctx
    typeOf ctx' t2
  (TermIf t1 t2 t3) -> do
    tyT1 <- typeOf ctx t1
    if tyT1 /= TyBool
      then fail "if conditional must be Bool"
      else do
        tyT2 <- typeOf ctx t2
        tyT3 <- typeOf ctx t3
        if tyT2 == tyT3
          then return tyT2
          else fail "if arms must have matching types"

typeOfCompOp :: Ctx -> Term -> Term -> Either String Ty
typeOfCompOp ctx t1 t2 = do
  tyT1 <- typeOf ctx t1
  tyT2 <- typeOf ctx t2
  if tyT1 == TyInt && tyT2 == TyInt
    then return TyBool
    else fail "comparison args must be Int"

typeOfUnaryLogicalOp :: Ctx -> Term -> Either String Ty
typeOfUnaryLogicalOp ctx t1 = do
  tyT1 <- typeOf ctx t1
  if tyT1 == TyBool then return TyBool else fail "logical op args must be Bool"

typeOfBinaryLogicalOp :: Ctx -> Term -> Term -> Either String Ty
typeOfBinaryLogicalOp ctx t1 t2 = do
  tyT1 <- typeOf ctx t1
  tyT2 <- typeOf ctx t2
  if tyT1 == TyBool && tyT2 == TyBool
    then return TyBool
    else fail "logical op args must be Bool"

typeOfUnaryArithOp :: Ctx -> Term -> Either String Ty
typeOfUnaryArithOp ctx t1 = do
  tyT1 <- typeOf ctx t1
  if tyT1 == TyInt
    then return TyInt
    else fail "arithmetic operator args must be Int"

typeOfBinaryArithOp :: Ctx -> Term -> Term -> Either String Ty
typeOfBinaryArithOp ctx t1 t2 = do
  tyT1 <- typeOf ctx t1
  tyT2 <- typeOf ctx t2
  if tyT1 == TyInt && tyT2 == TyInt
    then return TyInt
    else fail "arithmetic operator args must be Int"
