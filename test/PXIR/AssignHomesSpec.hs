module PXIR.AssignHomesSpec
  ( spec
  )
where

import qualified Data.Map                      as M
import qualified PXIR.AST                      as P
import           PXIR.AssignHomes
import           Test.Hspec

spec :: Spec
spec = do
  describe "assignHomesInBlock" $ do
    it "assigns vars to stack in basic add and neg example" $ basicAddAndNegSpec
    it "assigns vars to stack when move has src and dst var args"
      $ moveWithSrcDstVarsSpec
    it
        "assigns vars to regs and stack, reusing reg and stack locations for non-conflicting vars"
      $ assignToRegsAndStackSpec
    it "assigns vars to stack when there are multiple blocks" $ multBlocksSpec


basicAddAndNegSpec =
  assignHomes (P.Label "start") [] input `shouldBe` (expected, 8)
 where
  input = M.singleton
    (P.Label "start")
    [ P.InstrMovQ (P.ArgInt 10) (P.ArgVar (P.Var "_0"))
    , P.InstrNegQ (P.ArgVar (P.Var "_0"))
    , P.InstrMovQ (P.ArgInt 52) (P.ArgReg P.RegRAX)
    , P.InstrAddQ (P.ArgVar (P.Var "_0")) (P.ArgReg P.RegRAX)
    , P.InstrJmp (P.Label "conclusion")
    ]
  expected = M.singleton
    (P.Label "start")
    [ P.InstrMovQ (P.ArgInt 10) (P.ArgDeref P.RegRBP (-8))
    , P.InstrNegQ (P.ArgDeref P.RegRBP (-8))
    , P.InstrMovQ (P.ArgInt 52) (P.ArgReg P.RegRAX)
    , P.InstrAddQ (P.ArgDeref P.RegRBP (-8)) (P.ArgReg P.RegRAX)
    , P.InstrJmp (P.Label "conclusion")
    ]

moveWithSrcDstVarsSpec =
  assignHomes (P.Label "start") [] input `shouldBe` (expected, 8)
 where
  input = M.singleton
    (P.Label "start")
    [ P.InstrMovQ (P.ArgInt 10) (P.ArgVar (P.Var "x.1"))
    , P.InstrMovQ (P.ArgVar (P.Var "x.1")) (P.ArgVar (P.Var "x.2"))
    , P.InstrMovQ (P.ArgVar (P.Var "x.2")) (P.ArgReg P.RegRAX)
    , P.InstrJmp (P.Label "conclusion")
    ]
  expected = M.singleton
    (P.Label "start")
    [ P.InstrMovQ (P.ArgInt 10) (P.ArgDeref P.RegRBP (-8))
    , P.InstrMovQ (P.ArgDeref P.RegRBP (-8)) (P.ArgDeref P.RegRBP (-8))
    , P.InstrMovQ (P.ArgDeref P.RegRBP (-8)) (P.ArgReg P.RegRAX)
    , P.InstrJmp (P.Label "conclusion")
    ]

assignToRegsAndStackSpec =
  assignHomes (P.Label "start") [P.RegRCX] input `shouldBe` (expected, 16)
 where
  input = M.singleton
    (P.Label "start")
    [ P.InstrMovQ (P.ArgInt 1) (P.ArgVar (P.Var "v"))
    , P.InstrMovQ (P.ArgInt 46) (P.ArgVar (P.Var "w"))
    , P.InstrMovQ (P.ArgVar (P.Var "v")) (P.ArgVar (P.Var "x"))
    , P.InstrAddQ (P.ArgInt 7) (P.ArgVar (P.Var "w"))
    , P.InstrMovQ (P.ArgVar (P.Var "x")) (P.ArgVar (P.Var "y"))
    , P.InstrAddQ (P.ArgInt 4) (P.ArgVar (P.Var "y"))
    , P.InstrMovQ (P.ArgVar (P.Var "x")) (P.ArgVar (P.Var "z"))
    , P.InstrAddQ (P.ArgVar (P.Var "w")) (P.ArgVar (P.Var "z"))
    , P.InstrMovQ (P.ArgVar (P.Var "y")) (P.ArgVar (P.Var "t.1"))
    , P.InstrNegQ (P.ArgVar (P.Var "t.1"))
    , P.InstrMovQ (P.ArgVar (P.Var "z")) (P.ArgReg P.RegRAX)
    , P.InstrAddQ (P.ArgVar (P.Var "t.1")) (P.ArgReg P.RegRAX)
    , P.InstrJmp (P.Label "conclusion")
    ]
  expected = M.singleton
    (P.Label "start")
    [ P.InstrMovQ (P.ArgInt 1) (P.ArgReg P.RegRCX)
    , P.InstrMovQ (P.ArgInt 46) (P.ArgDeref P.RegRBP (-16))
    , P.InstrMovQ (P.ArgReg P.RegRCX) (P.ArgReg P.RegRCX)
    , P.InstrAddQ (P.ArgInt 7) (P.ArgDeref P.RegRBP (-16))
    , P.InstrMovQ (P.ArgReg P.RegRCX) (P.ArgDeref P.RegRBP (-8))
    , P.InstrAddQ (P.ArgInt 4) (P.ArgDeref P.RegRBP (-8))
    , P.InstrMovQ (P.ArgReg P.RegRCX) (P.ArgReg P.RegRCX)
    , P.InstrAddQ (P.ArgDeref P.RegRBP (-16)) (P.ArgReg P.RegRCX)
    , P.InstrMovQ (P.ArgDeref P.RegRBP (-8)) (P.ArgDeref P.RegRBP (-8))
    , P.InstrNegQ (P.ArgDeref P.RegRBP (-8))
    , P.InstrMovQ (P.ArgReg P.RegRCX) (P.ArgReg P.RegRAX)
    , P.InstrAddQ (P.ArgDeref P.RegRBP (-8)) (P.ArgReg P.RegRAX)
    , P.InstrJmp (P.Label "conclusion")
    ]

multBlocksSpec =
  assignHomes (P.Label "start") [P.RegRCX] input `shouldBe` (expected, 8)
 where
  input = M.fromList
    [ ( P.Label "start"
      , [ P.InstrMovQ (P.ArgInt 11) (P.ArgVar (P.Var "y"))
        , P.InstrCmpQ (P.ArgInt 0) (P.ArgVar (P.Var "y"))
        , P.InstrJmpIf P.CCE (P.Label "block1")
        , P.InstrJmp (P.Label "block2")
        ]
      )
    , ( P.Label "block0"
      , [ P.InstrMovQ (P.ArgVar (P.Var "x")) (P.ArgReg P.RegRAX)
        , P.InstrAddQ (P.ArgVar (P.Var "y")) (P.ArgReg P.RegRAX)
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
  expected = M.fromList
    [ ( P.Label "start"
      , [ P.InstrMovQ (P.ArgInt 11) (P.ArgReg P.RegRCX)
        , P.InstrCmpQ (P.ArgInt 0) (P.ArgReg P.RegRCX)
        , P.InstrJmpIf P.CCE (P.Label "block1")
        , P.InstrJmp (P.Label "block2")
        ]
      )
    , ( P.Label "block0"
      , [ P.InstrMovQ (P.ArgDeref P.RegRBP (-8)) (P.ArgReg P.RegRAX)
        , P.InstrAddQ (P.ArgReg P.RegRCX) (P.ArgReg P.RegRAX)
        , P.InstrJmp (P.Label "conclusion")
        ]
      )
    , ( P.Label "block1"
      , [ P.InstrMovQ (P.ArgInt 10) (P.ArgDeref P.RegRBP (-8))
        , P.InstrJmp (P.Label "block0")
        ]
      )
    , ( P.Label "block2"
      , [ P.InstrMovQ (P.ArgInt 32) (P.ArgDeref P.RegRBP (-8))
        , P.InstrJmp (P.Label "block0")
        ]
      )
    ]

