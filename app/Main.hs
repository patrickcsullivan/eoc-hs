module Main where

import           Driver
import qualified RIR.AST                       as R

main :: IO ()
main = putStrLn $ drive input

input :: R.Term
input = R.TermIf
  (R.TermIf (R.TermEq R.TermRead (R.TermInt 1))
            (R.TermEq R.TermRead (R.TermInt 0))
            (R.TermEq R.TermRead (R.TermInt 2))
  )
  (R.TermAdd (R.TermInt 10) (R.TermInt 32))
  (R.TermSub (R.TermInt 700) (R.TermInt 77))
