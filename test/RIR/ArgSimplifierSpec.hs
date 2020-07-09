module RIR.ArgSimplifierSpec
  ( spec
  )
where

import           RIR.AST
import           RIR.ArgSimplifier
import           Test.Hspec

spec :: Spec
spec = do
  describe "simplifyArgs" $ do
    it "doesn't change term where args are already simplified"
      $ alreadySimplifiedSpec
    it "simplifies args in a basic add and neg example" $ basicAddAndNegSpec
    it "simplifies arg to neg" $ simplifyNegArgSpec
    it "simplifies args to add" $ simplifyAddArgsSpec

alreadySimplifiedSpec = simplifyArgs inputTrm 0 `shouldBe` (expectedTrm, 0)
 where
  inputTrm = TermLet
    (Var "foo")
    (TermLet (Var "bar")
             (TermVal (ValueInt 10))
             (TermAdd (TermVal (ValueInt 10)) (TermVar (Var "bar")))
    )
    (TermNeg (TermVar (Var "foo")))
  expectedTrm = inputTrm

basicAddAndNegSpec = simplifyArgs inputTrm 5 `shouldBe` (expectedTrm, 6)
 where
  inputTrm = TermAdd (TermVal (ValueInt 52)) (TermNeg (TermVal (ValueInt 10)))
  expectedTrm = TermLet
    (Var "_5")
    (TermNeg (TermVal (ValueInt 10)))
    (TermAdd (TermVal (ValueInt 52)) (TermVar (Var "_5")))

simplifyNegArgSpec = simplifyArgs inputTrm 10 `shouldBe` (expectedTrm, 11)
 where
  inputTrm    = TermNeg TermRead
  expectedTrm = TermLet (Var "_10") TermRead (TermNeg (TermVar (Var "_10")))


simplifyAddArgsSpec = simplifyArgs inputTrm 0 `shouldBe` (expectedTrm, 6)
 where
  inputTrm = TermAdd
    (TermAdd (TermVal (ValueInt 1)) (TermVal (ValueInt 2)))
    (TermAdd (TermAdd (TermVal (ValueInt 3)) TermRead)
             (TermAdd TermRead (TermVal (ValueInt 4)))
    )
  expectedTrm = TermLet
    (Var "_0")
    (TermAdd (TermVal (ValueInt 1)) (TermVal (ValueInt 2)))
    (TermLet
      (Var "_1")
      (TermLet
        (Var "_2")
        (TermLet (Var "_3")
                 TermRead
                 (TermAdd (TermVal (ValueInt 3)) (TermVar (Var "_3")))
        )
        (TermLet
          (Var "_4")
          (TermLet (Var "_5")
                   TermRead
                   (TermAdd (TermVar (Var "_5")) (TermVal (ValueInt 4)))
          )
          (TermAdd (TermVar (Var "_2")) (TermVar (Var "_4")))
        )
      )
      (TermAdd (TermVar (Var "_0")) (TermVar (Var "_1")))
    )
