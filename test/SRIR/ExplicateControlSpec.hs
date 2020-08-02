module SRIR.ExplicateControlSpec
  ( spec
  )
where

import qualified CIR.AST                       as C
import qualified Data.Map                      as M
import qualified SRIR.AST                      as S
import           SRIR.ExplicateControl
import           Test.Hspec

spec :: Spec
spec = do
  describe "explicateControl" $ do
    it "explicates control in basic add and neg example" $ basicAddAndNegSpec
    it "explicates control for let binding and body" $ nestedLetAssignsSpec
    it "explicates control for if within nested if in predicate"
      $ nestedIfPredSpec

basicAddAndNegSpec =
  explicateControl inputTrm (C.Label "start") 3 `shouldBe` (expectedBlocks, 3)
 where
  inputTrm = S.TermLet (S.Var "_0")
                       (S.TermNeg (S.TermInt 10))
                       (S.TermAdd (S.TermInt 52) (S.TermVar (S.Var "_0")))
  expectedTail = C.TailSeq
    (C.StmtAssign (C.Var "_0") (C.TermNeg (C.ArgInt 10)))
    (C.TailRet (C.TermAdd (C.ArgInt 52) (C.ArgVar (C.Var "_0"))))
  expectedBlocks = M.singleton (C.Label "start") expectedTail

nestedLetAssignsSpec =
  explicateControl inputTrm (C.Label "start") 3 `shouldBe` (expectedBlocks, 3)
 where
  inputTrm = S.TermLet
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
  expectedTail = C.TailSeq
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
  expectedBlocks = M.singleton (C.Label "start") expectedTail

nestedIfPredSpec =
  explicateControl inputTrm (C.Label "start") 55 `shouldBe` (expectedBlocks, 63)
 where
  inputTrm = S.TermIf
    (S.TermIf
      (S.TermLet (S.Var "tmp52")
                 S.TermRead
                 (S.TermEq (S.TermVar (S.Var "tmp52")) (S.TermInt 1))
      )
      (S.TermLet (S.Var "tmp53")
                 S.TermRead
                 (S.TermEq (S.TermVar (S.Var "tmp53")) (S.TermInt 0))
      )
      (S.TermLet (S.Var "tmp54")
                 S.TermRead
                 (S.TermEq (S.TermVar (S.Var "tmp54")) (S.TermInt 2))
      )
    )
    (S.TermAdd (S.TermInt 10) (S.TermInt 32))
    (S.TermAdd (S.TermInt 700) (S.TermInt 77))
  startBlock = C.TailSeq
    (C.StmtAssign (C.Var "tmp52") C.TermRead)
    (C.TailIf (C.TermCmp C.CmpEq (C.ArgVar (C.Var "tmp52")) (C.ArgInt 1))
              (C.Label "block61")
              (C.Label "block62")
    )
  block55 = C.TailRet (C.TermAdd (C.ArgInt 10) (C.ArgInt 32))
  block56 = C.TailRet (C.TermAdd (C.ArgInt 700) (C.ArgInt 77))
  block57 = C.TailGoTo (C.Label "block55")
  block58 = C.TailGoTo (C.Label "block56")
  block59 = C.TailGoTo (C.Label "block55")
  block60 = C.TailGoTo (C.Label "block56")
  block61 = C.TailSeq
    (C.StmtAssign (C.Var "tmp53") C.TermRead)
    (C.TailIf (C.TermCmp C.CmpEq (C.ArgVar (C.Var "tmp53")) (C.ArgInt 0))
              (C.Label "block57")
              (C.Label "block58")
    )
  block62 = C.TailSeq
    (C.StmtAssign (C.Var "tmp54") C.TermRead)
    (C.TailIf (C.TermCmp C.CmpEq (C.ArgVar (C.Var "tmp54")) (C.ArgInt 2))
              (C.Label "block59")
              (C.Label "block60")
    )
  expectedBlocks = M.fromList
    [ (C.Label "start"  , startBlock)
    , (C.Label "block55", block55)
    , (C.Label "block56", block56)
    , (C.Label "block57", block57)
    , (C.Label "block58", block58)
    , (C.Label "block59", block59)
    , (C.Label "block60", block60)
    , (C.Label "block61", block61)
    , (C.Label "block62", block62)
    ]
