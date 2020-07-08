module RIR.UniquifierSpec
  ( spec
  )
where

import           RIR.AST
import           RIR.Uniquifier
import           Test.Hspec

spec :: Spec
spec = do
  describe "uniquify" $ do
    it "uniquifies shadowed vars" $ shadowedVarsSpec
    it "doesn't change term with no vars" $ noVarsSpec

shadowedVarsSpec = uniquify input `shouldBe` expected
 where
  input = TermLet
    (Var "myVar")
    (TermVal (ValueInt 42))
    (TermLet
      (Var "input")
      TermRead
      (TermLet
        (Var "myVar")
        (TermAdd (TermVar (Var "myVar")) (TermNeg (TermVar (Var "input"))))
        (TermVar (Var "myVar"))
      )
    )
  expected = TermLet
    (Var "_1")
    (TermVal (ValueInt 42))
    (TermLet
      (Var "_2")
      TermRead
      (TermLet (Var "_3")
               (TermAdd (TermVar (Var "_1")) (TermNeg (TermVar (Var "_2"))))
               (TermVar (Var "_3"))
      )
    )

noVarsSpec = uniquify input `shouldBe` expected
 where
  input    = TermAdd (TermVal (ValueInt 52)) (TermNeg (TermVal (ValueInt 10)))
  expected = input
