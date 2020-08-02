{-|
Module      : CIR.AST
Description : C-like Intermediate Representation
-}
module CIR.AST where

newtype Var = Var { unvar :: String } deriving (Show, Eq, Ord)

data Arg
    = ArgBool Bool
    | ArgInt Int
    | ArgVar Var
    deriving Eq

data Cmp
    = CmpEq
    | CmpLT
    deriving Eq

data Term
    = TermRead
    | TermArg Arg
    | TermNeg Arg
    | TermAdd Arg Arg
    | TermNot Arg
    | TermCmp Cmp Arg Arg
    deriving Eq

data Stmt
    = StmtAssign Var Term
    deriving Eq

newtype Label = Label { unlabel :: String } deriving (Eq, Ord)

data Tail
    = TailSeq Stmt Tail
    | TailRet Term
    | TailGoTo Label
    | TailIf Term Label Label
    deriving Eq

instance Show Arg where
  show arg = case arg of
    ArgBool b          -> show b
    ArgInt  n          -> show n
    ArgVar  (Var name) -> name

instance Show Cmp where
  show cmp = case cmp of
    CmpEq -> "eq?"
    CmpLT -> "<"

instance Show Term where
  show trm = case trm of
    TermRead              -> "read"
    TermArg arg           -> show arg
    TermNeg arg           -> "neg " ++ show arg
    TermAdd arg1 arg2     -> "add " ++ show arg1 ++ " " ++ show arg2
    TermNot arg           -> "not " ++ show arg
    TermCmp cmp arg1 arg2 -> show cmp ++ " " ++ show arg1 ++ " " ++ show arg2

instance Show Stmt where
  show (StmtAssign (Var name) trm) = name ++ " = " ++ show trm

instance Show Label where
  show (Label lbl) = lbl

instance Show Tail where
  show (TailSeq stmt tail) = show stmt ++ "\n" ++ show tail
  show (TailRet  trm     ) = "return " ++ show trm ++ "\n"
  show (TailGoTo lbl     ) = "goto " ++ show lbl ++ "\n"
  show (TailIf trm lbl1 lbl2) =
    "if ("
      ++ show trm
      ++ ")\n"
      ++ "  then goto "
      ++ show lbl1
      ++ "\n"
      ++ "  else goto "
      ++ show lbl2
      ++ "\n"
