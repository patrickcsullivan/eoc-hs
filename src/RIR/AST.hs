{-|
Module      : RIR.AST
Description : R Intermediate Representation

An intermediate representation that most closely corresponds to the AST of
source code.
-}
module RIR.AST where

newtype Var = Var { unvar :: String } deriving (Eq, Ord)

data Term
    = TermRead
    | TermInt Int
    | TermNeg Term
    | TermAdd Term Term
    | TermVar Var
    | TermLet Var Term Term
    deriving Eq

isVal :: Term -> Bool
isVal (TermInt _) = True
isVal _           = False

instance Show Var where
  show (Var name) = name

instance Show Term where
  show trm = indentTerm 0 trm

indentTerm :: Int -> Term -> String
indentTerm ws trm = replicate ws ' ' ++ trmStr
 where
  trmStr = case trm of
    TermRead          -> "read"
    TermInt n         -> show n
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
