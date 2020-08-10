module RIR.ParserSpec
  ( spec
  )
where

import           Data.Either                    ( isLeft )
import           RIR.AST
import           RIR.Parser                     ( parse )
import           Test.Hspec

spec :: Spec
spec = do
  describe "parser" $ do
    it "parses" $ parseSpec

parseSpec = parse input `shouldBe` Right expected
 where
  input
    = "(let ([my_var 42])\n\
        \    (let ([input (read)])\n\
        \        (let ([my_var (+ my_var (- input))])\n\
        \            my_var)))\n\
        \"
  expected = TermLet
    (Var "my_var")
    (TermInt 42)
    (TermLet
      (Var "input")
      TermRead
      (TermLet
        (Var "my_var")
        (TermAdd (TermVar (Var "my_var")) (TermNeg (TermVar (Var "input"))))
        (TermVar (Var "my_var"))
      )
    )
