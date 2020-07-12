{-|
Module      : RIR.AST
Description : R Intermediate Representation

An intermediate representation that most closely corresponds to the AST of
source code.
-}
module RIR.AST where

newtype Var = Var { unvar :: String } deriving (Show, Eq, Ord)

data Value
    = ValueInt Int
    deriving (Show, Eq)

data Term
    = TermRead
    | TermVal Value
    | TermNeg Term
    | TermAdd Term Term
    | TermVar Var
    | TermLet Var Term Term
    deriving (Show, Eq)
