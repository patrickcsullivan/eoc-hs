module CIR.SelectInstructionsSpec
  ( spec
  )
where

import qualified CIR.AST                       as C
import           CIR.SelectInstructions
import           Data.Map                      as M
import qualified PXIR.AST                      as P
import           Test.Hspec

spec :: Spec
spec = do
  describe "selectInstructions" $ do
    it "converts CIR that reads to PXIR" $ readSpec
    it "converts a CIR basic add and neg example to PXIR" $ basicAddAndNegSpec
    it "converts a CIR add example to PXIR" $ addSpec
    it "converts a CIR add example where the left arg matches the dst to PXIR"
      $ addInPlaceLeftArgSpec
    it "converts a CIR add example where the right arg matches the dst to PXIR"
      $ addInPlaceLeftArgSpec
    it "converts a CIR add example where both args matches the dst to PXIR"
      $ addInPlaceLeftArgSpec
    it "converts a CIR neg example to PXIR" $ negSpec
    it "converts a CIR neg example where the arg matches the dst to PXIR"
      $ negInPlaceSpec

readSpec = selectInstructions input `shouldBe` expected
 where
  input = M.singleton
    (C.Label "start")
    (C.TailSeq (C.StmtAssign (C.Var "x") C.TermRead)
               (C.TailRet (C.TermArg (C.ArgVar (C.Var "x"))))
    )
  expected = M.singleton
    (P.Label "start")
    [ P.InstrCallQ (P.Label "read_int")
    , P.InstrMovQ (P.ArgReg P.RegRAX) (P.ArgVar (P.Var "x"))
    , P.InstrMovQ (P.ArgVar (P.Var "x")) (P.ArgReg P.RegRAX)
    , P.InstrJmp (P.Label "conclusion")
    ]

basicAddAndNegSpec = selectInstructions input `shouldBe` expected
 where
  input = M.singleton
    (C.Label "start")
    (C.TailSeq (C.StmtAssign (C.Var "_0") (C.TermNeg (C.ArgInt 10)))
               (C.TailRet (C.TermAdd (C.ArgInt 52) (C.ArgVar (C.Var "_0"))))
    )
  expected = M.singleton
    (P.Label "start")
    [ P.InstrMovQ (P.ArgInt 10) (P.ArgVar (P.Var "_0"))
    , P.InstrNegQ (P.ArgVar (P.Var "_0"))
    , P.InstrMovQ (P.ArgInt 52) (P.ArgReg P.RegRAX)
    , P.InstrAddQ (P.ArgVar (P.Var "_0")) (P.ArgReg P.RegRAX)
    , P.InstrJmp (P.Label "conclusion")
    ]

addSpec = selectInstructions input `shouldBe` expected
 where
  input = M.singleton
    (C.Label "start")
    (C.TailSeq
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
    )
  expected = M.singleton
    (P.Label "start")
    [ P.InstrMovQ (P.ArgInt 20) (P.ArgVar (P.Var "x.1"))
    , P.InstrMovQ (P.ArgInt 22) (P.ArgVar (P.Var "x.2"))
    , P.InstrMovQ (P.ArgVar (P.Var "x.1")) (P.ArgVar (P.Var "y"))
    , P.InstrAddQ (P.ArgVar (P.Var "x.2")) (P.ArgVar (P.Var "y"))
    , P.InstrMovQ (P.ArgVar (P.Var "y")) (P.ArgReg P.RegRAX)
    , P.InstrJmp (P.Label "conclusion")
    ]

addInPlaceLeftArgSpec = selectInstructions input `shouldBe` expected
 where
  input = M.singleton
    (C.Label "start")
    (C.TailSeq
      (C.StmtAssign (C.Var "x") (C.TermArg (C.ArgInt 20)))
      (C.TailSeq
        (C.StmtAssign (C.Var "x")
                      (C.TermAdd (C.ArgVar (C.Var "x")) (C.ArgInt 22))
        )
        (C.TailRet (C.TermArg (C.ArgVar (C.Var "x"))))
      )
    )
  expected = M.singleton
    (P.Label "start")
    [ P.InstrMovQ (P.ArgInt 20) (P.ArgVar (P.Var "x"))
    , P.InstrAddQ (P.ArgInt 22) (P.ArgVar (P.Var "x"))
    , P.InstrMovQ (P.ArgVar (P.Var "x")) (P.ArgReg P.RegRAX)
    , P.InstrJmp (P.Label "conclusion")
    ]

addInPlaceLRightArgSpec = selectInstructions input `shouldBe` expected
 where
  input = M.singleton
    (C.Label "start")
    (C.TailSeq
      (C.StmtAssign (C.Var "x") (C.TermArg (C.ArgInt 20)))
      (C.TailSeq
        (C.StmtAssign (C.Var "x")
                      (C.TermAdd (C.ArgInt 22) (C.ArgVar (C.Var "x")))
        )
        (C.TailRet (C.TermArg (C.ArgVar (C.Var "x"))))
      )
    )
  expected = M.singleton
    (P.Label "start")
    [ P.InstrMovQ (P.ArgInt 20) (P.ArgVar (P.Var "x"))
    , P.InstrAddQ (P.ArgInt 22) (P.ArgVar (P.Var "x"))
    , P.InstrMovQ (P.ArgVar (P.Var "x")) (P.ArgReg P.RegRAX)
    , P.InstrJmp (P.Label "conclusion")
    ]

addInPlaceBothArgsSpec = selectInstructions input `shouldBe` expected
 where
  input = M.singleton
    (C.Label "start")
    (C.TailSeq
      (C.StmtAssign (C.Var "x") (C.TermArg (C.ArgInt 20)))
      (C.TailSeq
        (C.StmtAssign
          (C.Var "x")
          (C.TermAdd (C.ArgVar (C.Var "x")) (C.ArgVar (C.Var "x")))
        )
        (C.TailRet (C.TermArg (C.ArgVar (C.Var "x"))))
      )
    )
  expected = M.singleton
    (P.Label "start")
    [ P.InstrMovQ (P.ArgInt 20) (P.ArgVar (P.Var "x"))
    , P.InstrAddQ (P.ArgVar (P.Var "x")) (P.ArgVar (P.Var "x"))
    , P.InstrMovQ (P.ArgVar (P.Var "x")) (P.ArgReg P.RegRAX)
    , P.InstrJmp (P.Label "conclusion")
    ]

negSpec = selectInstructions input `shouldBe` expected
 where
  input = M.singleton
    (C.Label "start")
    (C.TailSeq
      (C.StmtAssign (C.Var "x") (C.TermArg (C.ArgInt 20)))
      (C.TailSeq (C.StmtAssign (C.Var "y") (C.TermNeg (C.ArgVar (C.Var "x"))))
                 (C.TailRet (C.TermArg (C.ArgVar (C.Var "y"))))
      )
    )
  expected = M.singleton
    (P.Label "start")
    [ P.InstrMovQ (P.ArgInt 20) (P.ArgVar (P.Var "x"))
    , P.InstrMovQ (P.ArgVar (P.Var "x")) (P.ArgVar (P.Var "y"))
    , P.InstrNegQ (P.ArgVar (P.Var "y"))
    , P.InstrMovQ (P.ArgVar (P.Var "y")) (P.ArgReg P.RegRAX)
    , P.InstrJmp (P.Label "conclusion")
    ]

negInPlaceSpec = selectInstructions input `shouldBe` expected
 where
  input = M.singleton
    (C.Label "start")
    (C.TailSeq
      (C.StmtAssign (C.Var "x") (C.TermArg (C.ArgInt 20)))
      (C.TailSeq (C.StmtAssign (C.Var "x") (C.TermNeg (C.ArgVar (C.Var "x"))))
                 (C.TailRet (C.TermArg (C.ArgVar (C.Var "x"))))
      )
    )
  expected = M.singleton
    (P.Label "start")
    [ P.InstrMovQ (P.ArgInt 20) (P.ArgVar (P.Var "x"))
    , P.InstrNegQ (P.ArgVar (P.Var "x"))
    , P.InstrMovQ (P.ArgVar (P.Var "x")) (P.ArgReg P.RegRAX)
    , P.InstrJmp (P.Label "conclusion")
    ]
