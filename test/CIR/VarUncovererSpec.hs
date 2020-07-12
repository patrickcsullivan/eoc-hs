module CIR.VarUncovererSpec
  ( spec
  )
where

import qualified CIR.AST                       as C
import           CIR.VarUncoverer
import           Test.Hspec

spec :: Spec
spec = do
  describe "uncoverVars" $ do
    it "uncovers vars from a basic add and neg example" $ basicAddAndNegSpec
    it "uncovers vars from a basic add example" $ basicAddSpec

basicAddAndNegSpec = uncoverVars input `shouldBe` expected
 where
  input = C.TailSeq
    (C.StmtAssign (C.Var "_0") (C.TermNeg (C.ArgInt 10)))
    (C.TailRet (C.TermAdd (C.ArgInt 52) (C.ArgVar (C.Var "_0"))))
  expected = [C.Var "_0"]

basicAddSpec = uncoverVars input `shouldBe` expected
 where
  input = C.TailSeq
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
  expected = [(C.Var "x.1"), (C.Var "x.2"), (C.Var "y")]
