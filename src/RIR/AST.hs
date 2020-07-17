{-|
Module      : RIR.AST
Description : R Intermediate Representation

An intermediate representation that most closely corresponds to the AST of
source code.
-}
module RIR.AST where

newtype Var = Var { unvar :: String } deriving (Eq, Ord)

data Value
    = ValueInt Int
    deriving Eq

data Term
    = TermRead
    | TermVal Value
    | TermNeg Term
    | TermAdd Term Term
    | TermVar Var
    | TermLet Var Term Term
    deriving Eq

instance Show Var where
  show (Var name) = name

instance Show Value where
  show (ValueInt n) = show n

instance Show Term where
  show trm = indentTerm 0 trm

indentTerm :: Int -> Term -> String
indentTerm ws trm = replicate ws ' ' ++ trmStr
 where
  trmStr = case trm of
    TermRead          -> "read"
    TermVal val       -> show val
    TermNeg trm       -> "(- " ++ show trm ++ ")"
    TermAdd trm1 trm2 -> "(+ " ++ show trm1 ++ " " ++ show trm2 ++ ")"
    TermVar var       -> show var
    TermLet var bnd bdy ->
      "(let (["
        ++ show var
        ++ ",\n"
        ++ indentTerm (ws + 2) bnd
        ++ ")]\n"
        ++ indentTerm (ws + 2) bdy
        ++ ")"
