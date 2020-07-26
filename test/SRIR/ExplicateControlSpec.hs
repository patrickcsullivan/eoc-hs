module SRIR.ExplicateControlSpec
  ( spec
  )
where

import qualified CIR.AST                       as C
import qualified SRIR.AST                      as S
import           SRIR.ExplicateControl
import           Test.Hspec

spec :: Spec
spec = do
  describe "explicateControl" $ do
    it "explicates control in basic add and neg example" $ basicAddAndNegSpec
    it "explicates control for let binding and body" $ nestedLetAssignsSpec

basicAddAndNegSpec = explicateControl input `shouldBe` expected
 where
  input = S.TermLet (S.Var "_0")
                    (S.TermNeg (S.TermInt 10))
                    (S.TermAdd (S.TermInt 52) (S.TermVar (S.Var "_0")))
  expected = C.TailSeq
    (C.StmtAssign (C.Var "_0") (C.TermNeg (C.ArgInt 10)))
    (C.TailRet (C.TermAdd (C.ArgInt 52) (C.ArgVar (C.Var "_0"))))

nestedLetAssignsSpec = explicateControl input `shouldBe` expected
 where
  input = S.TermLet
    (S.Var "y")
    (S.TermLet
      (S.Var "x.1")
      (S.TermInt 20)
      (S.TermLet
        (S.Var "x.2")
        (S.TermInt 22)
        (S.TermAdd (S.TermVar (S.Var "x.1")) (S.TermVar (S.Var "x.2")))
      )
    )
    (S.TermVar (S.Var "y"))
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
