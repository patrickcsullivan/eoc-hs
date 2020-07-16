{-|
Module      : CIR.AST
Description : C-like Intermediate Representation
-}
module CIR.AST where

newtype Var = Var { unvar :: String } deriving (Show, Eq, Ord)

data Arg
    = ArgInt Int
    | ArgVar Var
    deriving Eq

data Term
    = TermRead
    | TermArg Arg
    | TermNeg Arg
    | TermAdd Arg Arg
    deriving Eq

data Stmt
    = StmtAssign Var Term
    deriving Eq

data Tail
    = TailSeq Stmt Tail
    | TailRet Term
    deriving  Eq

instance Show Arg where
  show arg = case arg of
    ArgInt n          -> show n
    ArgVar (Var name) -> name

instance Show Term where
  show trm = case trm of
    TermRead          -> "read"
    TermArg arg       -> show arg
    TermNeg arg       -> "neg " ++ show arg
    TermAdd arg1 arg2 -> "add " ++ show arg1 ++ show arg2

instance Show Stmt where
  show (StmtAssign (Var name) trm) = name ++ " = " ++ show trm

instance Show Tail where
  show (TailSeq stmt tail) = show stmt ++ "\n" ++ show tail
  show (TailRet trm      ) = "return " ++ show trm ++ "\n"
