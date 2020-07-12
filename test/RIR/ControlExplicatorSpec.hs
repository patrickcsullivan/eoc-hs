module RIR.ControlExplicatorSpec
  ( spec
  )
where

import qualified CIR.AST                       as C
import qualified RIR.AST                       as R
import           RIR.ControlExplicator
import           Test.Hspec

spec :: Spec
spec = do
  describe "explicateControl" $ do
    it "explicates control in basic add and neg example" $ basicAddAndNegSpec
    it "explicates control for let binding and body" $ nestedLetAssignsSpec

basicAddAndNegSpec = explicateControl input `shouldBe` expected
 where
  input = R.TermLet
    (R.Var "_0")
    (R.TermNeg (R.TermVal (R.ValueInt 10)))
    (R.TermAdd (R.TermVal (R.ValueInt 52)) (R.TermVar (R.Var "_0")))
  expected = C.TailSeq
    (C.StmtAssign (C.Var "_0") (C.TermNeg (C.ArgInt 10)))
    (C.TailRet (C.TermAdd (C.ArgInt 52) (C.ArgVar (C.Var "_0"))))

nestedLetAssignsSpec = explicateControl input `shouldBe` expected
 where
  input = R.TermLet
    (R.Var "y")
    (R.TermLet
      (R.Var "x.1")
      (R.TermVal (R.ValueInt 20))
      (R.TermLet
        (R.Var "x.2")
        (R.TermVal (R.ValueInt 22))
        (R.TermAdd (R.TermVar (R.Var "x.1")) (R.TermVar (R.Var "x.2")))
      )
    )
    (R.TermVar (R.Var "y"))
  expected = C.TailSeq
    (C.StmtAssign (C.Var "x.1") (C.TermArg (C.ArgInt 20)))
    (C.TailSeq
      (C.StmtAssign (C.Var "x.2") (C.TermArg (C.ArgInt 22)))
      (C.TailSeq
        (C.StmtAssign
          (C.Var "y")
          (C.TermAdd (C.ArgVar (C.Var "x.1")) (C.ArgVar (C.Var "x.2")))
        )
        (C.TailRet (C.TermArg (C.ArgVar (C.Var "y"))))
      )
    )
