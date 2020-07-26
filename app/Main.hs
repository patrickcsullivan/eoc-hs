module Main where

import           Driver
import qualified SRIR.AST                      as S


main :: IO ()
main = putStrLn $ drive trm

trm = S.TermLet
  (S.Var "my_var")
  (S.TermInt 42)
  (S.TermLet
    (S.Var "input")
    S.TermRead
    (S.TermLet
      (S.Var "my_var")
      (S.TermAdd (S.TermVar (S.Var "my_var"))
                 (S.TermNeg (S.TermVar (S.Var "input")))
      )
      (S.TermVar (S.Var "my_var"))
    )
  )
