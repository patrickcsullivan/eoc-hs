module RIR.UniquifyArgsSpec
  ( spec
  )
where

import           RIR.AST
import           RIR.UniquifyArgs
import           Test.Hspec

spec :: Spec
spec = do
  describe "uniquifyArgs" $ do
    it "uniquifies shadowed vars" $ shadowedVarsSpec
    it "doesn't change term with no vars" $ noVarsSpec

shadowedVarsSpec = uniquifyArgs inputTrm 10 `shouldBe` (expectedTrm, 13)
 where
  inputTrm = TermLet
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
  expectedTrm = TermLet
    (Var "_10")
    (TermVal (ValueInt 42))
    (TermLet
      (Var "_11")
      TermRead
      (TermLet (Var "_12")
               (TermAdd (TermVar (Var "_10")) (TermNeg (TermVar (Var "_11"))))
               (TermVar (Var "_12"))
      )
    )

noVarsSpec = uniquifyArgs inputTrm 0 `shouldBe` (inputTrm, 0)
 where
  inputTrm = TermAdd (TermVal (ValueInt 52)) (TermNeg (TermVal (ValueInt 10)))
