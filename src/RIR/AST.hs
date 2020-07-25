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
    -- Value terms
    | TermBool Bool
    | TermInt Int
    -- Integer comparison operators
    | TermEq Term Term
    | TermLT Term Term
    | TermLTE Term Term
    | TermGT Term Term
    | TermGTE Term Term
    -- Logical operators
    | TermAnd Term Term
    | TermOr Term Term
    | TermNot Term
    -- Arithmetic operators
    | TermNeg Term
    | TermAdd Term Term
    -- Language constructs
    | TermVar Var
    | TermLet Var Term Term
    | TermIf Term Term Term
    deriving Eq

isVal :: Term -> Bool
isVal (TermBool _) = True
isVal (TermInt  _) = True
isVal _            = False

instance Show Var where
  show (Var name) = name

instance Show Term where
  show trm = indentTerm 0 trm

indentTerm :: Int -> Term -> String
indentTerm ws trm = replicate ws ' ' ++ trmStr
 where
  trmStr = case trm of
    TermRead      -> "read"
    -- Value terms
    TermBool b    -> show b
    TermInt  n    -> show n
    -- Integer comparison operators
    TermEq  t1 t2 -> operator "eq?" [t1, t2]
    TermLT  t1 t2 -> operator "<" [t1, t2]
    TermLTE t1 t2 -> operator "<=" [t1, t2]
    TermGT  t1 t2 -> operator ">" [t1, t2]
    TermGTE t1 t2 -> operator ">=" [t1, t2]
    -- Logical operators
    TermAnd t1 t2 -> operator "and" [t1, t2]
    TermOr  t1 t2 -> operator "or" [t1, t2]
    TermNot t1    -> operator "not" [t1]
    -- Arithmetic operators
    TermNeg t1    -> operator "-" [t1]
    TermAdd t1 t2 -> operator "+" [t1, t2]
    -- Language constructs
    TermVar var   -> show var
    TermLet var bnd bdy ->
      "(let (["
        ++ show var
        ++ ",\n"
        ++ indentTerm (ws + 2) bnd
        ++ ")]\n"
        ++ indentTerm (ws + 2) bdy
        ++ ")"
    TermIf t1 t2 t3 ->
      "(if\n"
        ++ indentTerm (ws + 2) t1
        ++ indentTerm (ws + 2) t2
        ++ indentTerm (ws + 2) t2
        ++ ")"

operator :: String -> [Term] -> String
operator name args = "(" ++ name ++ (unwords $ map show args) ++ ")"
