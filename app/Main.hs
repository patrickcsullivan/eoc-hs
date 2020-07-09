module Main where

import           Lib
import           RIR.AST
import           RIR.Uniquifier                 ( uniquify )

main :: IO ()
main = print $ uniquify input 0

input :: Term
input = TermLet
  (Var "myVar")
  (TermVal $ ValueInt 42)
  (TermLet
    (Var "input")
    TermRead
    (TermLet
      (Var "myVar")
      (TermAdd (TermVar $ Var "myVar") (TermNeg (TermVar $ Var "input")))
      (TermVar $ Var "myVar")
    )
  )

actual :: Term
actual = TermLet
  (Var { unvar = "_1" })
  (TermVal (ValueInt 42))
  (TermLet
    (Var { unvar = "_2" })
    TermRead
    (TermLet
      (Var { unvar = "_3" })
      (TermAdd (TermVar (Var { unvar = "_1" }))
               (TermNeg (TermVar (Var { unvar = "_2" })))
      )
      (TermVar (Var { unvar = "_3" }))
    )
  )
