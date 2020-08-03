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
    it "converts a CIR not example to PXIR" $ notSpec
    it "converts a CIR not example where the arg matches the dst to PXIR"
      $ notInPlaceSpec
    it "converts a CIR compare == example to PXIR" $ cmpEqSpec
    it "converts a CIR compare < example to PXIR" $ cmpLTSpec
    it "converts a CIR example with if and goto control flow to PXIR"
      $ ifAndGoToSpec
    it "converts a CIR if example with comparision predicate to PXIR"
      $ ifCmpPredSpec
    it "converts a CIR if example with arg predicate to PXIR" $ ifArgPredSpec
    it "converts a CIR if example with not predicate to PXIR" $ ifNotPredSpec
    it "converts a CIR if example with read predicate to PXIR" $ ifReadPredSpec
    it "converts a CIR if example with neg predicate to PXIR" $ ifNegPredSpec
    it "converts a CIR if example with add predicate to PXIR" $ ifAddPredSpec

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

notSpec = selectInstructions input `shouldBe` expected
 where
  input = M.singleton
    (C.Label "start")
    (C.TailSeq
      (C.StmtAssign (C.Var "x") (C.TermArg (C.ArgBool False)))
      (C.TailSeq (C.StmtAssign (C.Var "y") (C.TermNot (C.ArgVar (C.Var "x"))))
                 (C.TailRet (C.TermArg (C.ArgVar (C.Var "y"))))
      )
    )
  expected = M.singleton
    (P.Label "start")
    [ P.InstrMovQ (P.ArgInt 0) (P.ArgVar (P.Var "x"))
    , P.InstrMovQ (P.ArgVar (P.Var "x")) (P.ArgVar (P.Var "y"))
    , P.InstrXOrQ (P.ArgInt 1) (P.ArgVar (P.Var "y"))
    , P.InstrMovQ (P.ArgVar (P.Var "y")) (P.ArgReg P.RegRAX)
    , P.InstrJmp (P.Label "conclusion")
    ]

notInPlaceSpec = selectInstructions input `shouldBe` expected
 where
  input = M.singleton
    (C.Label "start")
    (C.TailSeq
      (C.StmtAssign (C.Var "x") (C.TermArg (C.ArgBool False)))
      (C.TailSeq (C.StmtAssign (C.Var "x") (C.TermNot (C.ArgVar (C.Var "x"))))
                 (C.TailRet (C.TermArg (C.ArgVar (C.Var "x"))))
      )
    )
  expected = M.singleton
    (P.Label "start")
    [ P.InstrMovQ (P.ArgInt 0) (P.ArgVar (P.Var "x"))
    , P.InstrXOrQ (P.ArgInt 1) (P.ArgVar (P.Var "x"))
    , P.InstrMovQ (P.ArgVar (P.Var "x")) (P.ArgReg P.RegRAX)
    , P.InstrJmp (P.Label "conclusion")
    ]

cmpEqSpec = selectInstructions input `shouldBe` expected
 where
  input = M.singleton
    (C.Label "start")
    (C.TailSeq
      (C.StmtAssign (C.Var "x") (C.TermArg (C.ArgInt 10)))
      (C.TailSeq
        (C.StmtAssign (C.Var "y")
                      (C.TermCmp C.CmpEq (C.ArgInt 10) (C.ArgVar (C.Var "x")))
        )
        (C.TailRet (C.TermArg (C.ArgVar (C.Var "y"))))
      )
    )
  expected = M.singleton
    (P.Label "start")
    [ P.InstrMovQ (P.ArgInt 10) (P.ArgVar (P.Var "x"))
    , P.InstrCmpQ (P.ArgVar (P.Var "x")) (P.ArgInt 10)
    , P.InstrSet P.CCE P.ByteRegAL
    , P.InstrMovZBQ P.ByteRegAL (P.ArgVar (P.Var "y"))
    , P.InstrMovQ (P.ArgVar (P.Var "y")) (P.ArgReg P.RegRAX)
    , P.InstrJmp (P.Label "conclusion")
    ]

cmpLTSpec = selectInstructions input `shouldBe` expected
 where
  input = M.singleton
    (C.Label "start")
    (C.TailSeq
      (C.StmtAssign (C.Var "x") (C.TermArg (C.ArgInt 10)))
      (C.TailSeq
        (C.StmtAssign (C.Var "y")
                      (C.TermCmp C.CmpLT (C.ArgInt 10) (C.ArgVar (C.Var "x")))
        )
        (C.TailRet (C.TermArg (C.ArgVar (C.Var "y"))))
      )
    )
  expected = M.singleton
    (P.Label "start")
    [ P.InstrMovQ (P.ArgInt 10) (P.ArgVar (P.Var "x"))
    , P.InstrCmpQ (P.ArgVar (P.Var "x")) (P.ArgInt 10)
    , P.InstrSet P.CCL P.ByteRegAL
    , P.InstrMovZBQ P.ByteRegAL (P.ArgVar (P.Var "y"))
    , P.InstrMovQ (P.ArgVar (P.Var "y")) (P.ArgReg P.RegRAX)
    , P.InstrJmp (P.Label "conclusion")
    ]

ifAndGoToSpec = selectInstructions input `shouldBe` expected
 where
  input = M.fromList
    [ ( C.Label "start"
      , (C.TailIf (C.TermCmp C.CmpEq (C.ArgVar (C.Var "y")) (C.ArgInt 0))
                  (C.Label "block1")
                  (C.Label "block2")
        )
      )
    , ( C.Label "block0"
      , C.TailRet (C.TermAdd (C.ArgVar (C.Var "x")) (C.ArgInt 100))
      )
    , ( C.Label "block1"
      , C.TailSeq (C.StmtAssign (C.Var "x") (C.TermArg (C.ArgInt 10)))
                  (C.TailGoTo (C.Label "block0"))
      )
    , ( C.Label "block2"
      , C.TailSeq (C.StmtAssign (C.Var "x") (C.TermArg (C.ArgInt 32)))
                  (C.TailGoTo (C.Label "block0"))
      )
    ]
  expected = M.fromList
    [ ( P.Label "start"
      , [ P.InstrCmpQ (P.ArgInt 0) (P.ArgVar (P.Var "y"))
        , P.InstrJmpIf P.CCE (P.Label "block1")
        , P.InstrJmp (P.Label "block2")
        ]
      )
    , ( P.Label "block0"
      , [ P.InstrMovQ (P.ArgVar (P.Var "x")) (P.ArgReg P.RegRAX)
        , P.InstrAddQ (P.ArgInt 100) (P.ArgReg P.RegRAX)
        , P.InstrJmp (P.Label "conclusion")
        ]
      )
    , ( P.Label "block1"
      , [ P.InstrMovQ (P.ArgInt 10) (P.ArgVar (P.Var "x"))
        , P.InstrJmp (P.Label "block0")
        ]
      )
    , ( P.Label "block2"
      , [ P.InstrMovQ (P.ArgInt 32) (P.ArgVar (P.Var "x"))
        , P.InstrJmp (P.Label "block0")
        ]
      )
    ]

ifCmpPredSpec = selectInstructions input `shouldBe` expected
 where
  input = M.fromList
    [ ( C.Label "start"
      , (C.TailIf
          (C.TermCmp C.CmpLT (C.ArgVar (C.Var "x")) (C.ArgVar (C.Var "y")))
          (C.Label "block1")
          (C.Label "block2")
        )
      )
    ]
  expected = M.fromList
    [ ( P.Label "start"
      , [ P.InstrCmpQ (P.ArgVar (P.Var "y")) (P.ArgVar (P.Var "x"))
        , P.InstrJmpIf P.CCL (P.Label "block1")
        , P.InstrJmp (P.Label "block2")
        ]
      )
    ]

ifArgPredSpec = selectInstructions input `shouldBe` expected
 where
  input = M.fromList
    [ ( C.Label "start"
      , (C.TailIf (C.TermArg (C.ArgVar (C.Var "x")))
                  (C.Label "block1")
                  (C.Label "block2")
        )
      )
    ]
  expected = M.fromList
    [ ( P.Label "start"
      , [ P.InstrCmpQ (P.ArgInt 0) (P.ArgVar (P.Var "x"))
        , P.InstrJmpIf P.CCE (P.Label "block2")
        , P.InstrJmp (P.Label "block1")
        ]
      )
    ]

ifNotPredSpec = selectInstructions input `shouldBe` expected
 where
  input = M.fromList
    [ ( C.Label "start"
      , (C.TailIf (C.TermNot (C.ArgVar (C.Var "x")))
                  (C.Label "block1")
                  (C.Label "block2")
        )
      )
    ]
  expected = M.fromList
    [ ( P.Label "start"
      , [ P.InstrCmpQ (P.ArgInt 0) (P.ArgVar (P.Var "x"))
        , P.InstrJmpIf P.CCE (P.Label "block1")
        , P.InstrJmp (P.Label "block2")
        ]
      )
    ]

ifReadPredSpec = selectInstructions input `shouldBe` expected
 where
  input = M.fromList
    [ ( C.Label "start"
      , (C.TailIf (C.TermRead) (C.Label "block1") (C.Label "block2"))
      )
    ]
  expected = M.fromList
    [ ( P.Label "start"
      , [ P.InstrCallQ (P.Label "read_int")
        , P.InstrCmpQ (P.ArgInt 0) (P.ArgReg P.RegRAX)
        , P.InstrJmpIf P.CCE (P.Label "block2")
        , P.InstrJmp (P.Label "block1")
        ]
      )
    ]

ifNegPredSpec = selectInstructions input `shouldBe` expected
 where
  input = M.fromList
    [ ( C.Label "start"
      , (C.TailIf (C.TermNeg (C.ArgVar (C.Var "x")))
                  (C.Label "block1")
                  (C.Label "block2")
        )
      )
    ]
  expected = M.fromList
    [ ( P.Label "start"
      , [ P.InstrMovQ (P.ArgVar (P.Var "x")) (P.ArgReg P.RegRAX)
        , P.InstrNegQ (P.ArgReg P.RegRAX)
        , P.InstrCmpQ (P.ArgInt 0) (P.ArgReg P.RegRAX)
        , P.InstrJmpIf P.CCE (P.Label "block2")
        , P.InstrJmp (P.Label "block1")
        ]
      )
    ]


ifAddPredSpec = selectInstructions input `shouldBe` expected
 where
  input = M.fromList
    [ ( C.Label "start"
      , (C.TailIf (C.TermAdd (C.ArgVar (C.Var "x")) (C.ArgVar (C.Var "y")))
                  (C.Label "block1")
                  (C.Label "block2")
        )
      )
    ]
  expected = M.fromList
    [ ( P.Label "start"
      , [ P.InstrMovQ (P.ArgVar (P.Var "x")) (P.ArgReg P.RegRAX)
        , P.InstrAddQ (P.ArgVar (P.Var "y")) (P.ArgReg P.RegRAX)
        , P.InstrCmpQ (P.ArgInt 0) (P.ArgReg P.RegRAX)
        , P.InstrJmpIf P.CCE (P.Label "block2")
        , P.InstrJmp (P.Label "block1")
        ]
      )
    ]
