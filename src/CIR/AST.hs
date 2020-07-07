module CIR.AST where

newtype Sym = Sym String deriving Show

data Arg
    = ArgInt Int
    | ArgVar Sym
    deriving Show

data Expr
    = ExprRead
    | ExprArg Arg
    | ExprNeg Arg
    | ExprAdd Arg Arg
    deriving Show

data Stmt
    = StmtAssign Sym Expr

data Tail
    = TailSeq Stmt Tail
    | TailRet Expr
