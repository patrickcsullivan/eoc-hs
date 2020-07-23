module Main where

import           Driver
import qualified RIR.AST                       as R


main :: IO ()
main = putStrLn $ drive trm

trm = R.TermLet
  (R.Var "my_var")
  (R.TermVal (R.ValueInt 42))
  (R.TermLet
    (R.Var "input")
    R.TermRead
    (R.TermLet
      (R.Var "my_var")
      (R.TermAdd (R.TermVar (R.Var "my_var"))
                 (R.TermNeg (R.TermVar (R.Var "input")))
      )
      (R.TermVar (R.Var "my_var"))
    )
  )
