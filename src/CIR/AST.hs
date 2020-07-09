module CIR.AST where

newtype Var = Var { unvar :: String } deriving (Show, Eq, Ord)

data Arg
    = ArgInt Int
    | ArgVar Var
    deriving (Show, Eq)

data Term
    = TermRead
    | TermArg Arg
    | TermNeg Arg
    | TermAdd Arg Arg
    deriving (Show, Eq)

data Stmt
    = StmtAssign Var Term
    deriving (Show, Eq)

data Tail
    = TailSeq Stmt Tail
    | TailRet Term
    deriving (Show, Eq)
