module RIR.Parser.Transform
  ( transform
  )
where

import qualified Data.Char                     as C
import qualified RIR.AST                       as R
import qualified RIR.Parser.STerm              as S

transform :: S.Term -> Either String R.Term
transform trm = case trm of
  S.TermAtom a | isInt a -> Right $ R.TermInt (read a)
  S.TermAtom "#t"        -> Right $ R.TermBool True
  S.TermAtom "#f"        -> Right $ R.TermBool False
  S.TermAtom a           -> do
    v <- transformVar a
    return $ R.TermVar v
  S.TermList (S.TermAtom "read" : args) -> transformOp0 R.TermRead args
  S.TermList (S.TermAtom "="    : args) -> transformOp2 R.TermEq args
  S.TermList (S.TermAtom "<"    : args) -> transformOp2 R.TermLT args
  S.TermList (S.TermAtom "<="   : args) -> transformOp2 R.TermLTE args
  S.TermList (S.TermAtom ">"    : args) -> transformOp2 R.TermGT args
  S.TermList (S.TermAtom ">="   : args) -> transformOp2 R.TermGTE args
  S.TermList (S.TermAtom "and"  : args) -> transformOp2 R.TermAnd args
  S.TermList (S.TermAtom "or"   : args) -> transformOp2 R.TermOr args
  S.TermList (S.TermAtom "not"  : args) -> transformOp1 R.TermNot args
  S.TermList (S.TermAtom "+"    : args) -> transformOp2 R.TermAdd args
  S.TermList (S.TermAtom "-"    : args) -> case args of
    [a1] -> do
      t1 <- transform a1
      return $ R.TermNeg t1
    [a1, a2] -> do
      t1 <- transform a1
      t2 <- transform a2
      return $ R.TermSub t1 t2
    _ -> Left "expects 1 or 2 args"
  S.TermList (S.TermAtom "if" : args) -> transformOp3 R.TermIf args
  S.TermList [S.TermAtom "let", S.TermList [S.TermList [S.TermAtom var, bnd]], bdy]
    -> do
      rVar <- transformVar var
      rBnd <- transform bnd
      rBdy <- transform bdy
      return $ R.TermLet rVar rBnd rBdy
  _ -> Left "invalid syntax"

transformVar :: String -> Either String R.Var
transformVar (first : rest) = if isFirstOK && areRestOK
  then Right $ R.Var (first : rest)
  else Left "invalid var name"
 where
  isFirstOK = (C.isLetter first) || (isSymbol first)
  areRestOK = all (\c -> C.isLetter c || C.isDigit c || isSymbol c) rest
  isSymbol c = c `elem` "!#$%&|*+-/:<=>?@^_~"

transformOp0 :: R.Term -> [S.Term] -> Either String R.Term
transformOp0 mkOp args = case args of
  [] -> return $ mkOp
  _  -> Left "expects 0 args"

transformOp1 :: (R.Term -> R.Term) -> [S.Term] -> Either String R.Term
transformOp1 mkOp args = case args of
  [a1] -> do
    t1 <- transform a1
    return $ mkOp t1
  _ -> Left "expects 1 arg"

transformOp2 :: (R.Term -> R.Term -> R.Term) -> [S.Term] -> Either String R.Term
transformOp2 mkOp args = case args of
  [a1, a2] -> do
    t1 <- transform a1
    t2 <- transform a2
    return $ mkOp t1 t2
  _ -> Left "expects 2 args"

transformOp3
  :: (R.Term -> R.Term -> R.Term -> R.Term) -> [S.Term] -> Either String R.Term
transformOp3 mkOp args = case args of
  [a1, a2, a3] -> do
    t1 <- transform a1
    t2 <- transform a2
    t3 <- transform a3
    return $ mkOp t1 t2 t3
  _ -> Left "expects 3 args"

isInt :: String -> Bool
isInt = all C.isDigit

