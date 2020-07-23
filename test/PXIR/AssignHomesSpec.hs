module PXIR.AssignHomesSpec
  ( spec
  )
where

import qualified PXIR.AST                      as P
import           PXIR.AssignHomes
import           Test.Hspec

spec :: Spec
spec = do
  describe "assignHomesInBlock" $ do
    it "assigns vars to stack in basic add and neg example" $ basicAddAndNegSpec
    it "assigns vars to stack when move has src and dst var args"
      $ moveWithSrcDstVars
    it
        "assigns vars to regs and stack, reusing reg and stack locations for non-conflicting vars"
      $ assignToRegsAndStackSpec


basicAddAndNegSpec = assignHomes [] input `shouldBe` (expected, 8)
 where
  input =
    [ P.InstrMovq (P.ArgInt 10) (P.ArgVar (P.Var "_0"))
    , P.InstrNegq (P.ArgVar (P.Var "_0"))
    , P.InstrMovq (P.ArgInt 52) (P.ArgReg P.RegRAX)
    , P.InstrAddq (P.ArgVar (P.Var "_0")) (P.ArgReg P.RegRAX)
    , P.InstrJumpq (P.Label "conclusion")
    ]
  expected =
    [ P.InstrMovq (P.ArgInt 10) (P.ArgDeref P.RegRBP (-8))
    , P.InstrNegq (P.ArgDeref P.RegRBP (-8))
    , P.InstrMovq (P.ArgInt 52) (P.ArgReg P.RegRAX)
    , P.InstrAddq (P.ArgDeref P.RegRBP (-8)) (P.ArgReg P.RegRAX)
    , P.InstrJumpq (P.Label "conclusion")
    ]

moveWithSrcDstVars = assignHomes [] input `shouldBe` (expected, 8)
 where
  input =
    [ P.InstrMovq (P.ArgInt 10) (P.ArgVar (P.Var "x.1"))
    , P.InstrMovq (P.ArgVar (P.Var "x.1")) (P.ArgVar (P.Var "x.2"))
    , P.InstrMovq (P.ArgVar (P.Var "x.2")) (P.ArgReg P.RegRAX)
    , P.InstrJumpq (P.Label "conclusion")
    ]
  expected =
    [ P.InstrMovq (P.ArgInt 10) (P.ArgDeref P.RegRBP (-8))
    , P.InstrMovq (P.ArgDeref P.RegRBP (-8)) (P.ArgDeref P.RegRBP (-8))
    , P.InstrMovq (P.ArgDeref P.RegRBP (-8)) (P.ArgReg P.RegRAX)
    , P.InstrJumpq (P.Label "conclusion")
    ]

assignToRegsAndStackSpec =
  assignHomes [P.RegRCX] input `shouldBe` (expected, 16)
 where
  input =
    [ P.InstrMovq (P.ArgInt 1) (P.ArgVar (P.Var "v"))
    , P.InstrMovq (P.ArgInt 46) (P.ArgVar (P.Var "w"))
    , P.InstrMovq (P.ArgVar (P.Var "v")) (P.ArgVar (P.Var "x"))
    , P.InstrAddq (P.ArgInt 7) (P.ArgVar (P.Var "w"))
    , P.InstrMovq (P.ArgVar (P.Var "x")) (P.ArgVar (P.Var "y"))
    , P.InstrAddq (P.ArgInt 4) (P.ArgVar (P.Var "y"))
    , P.InstrMovq (P.ArgVar (P.Var "x")) (P.ArgVar (P.Var "z"))
    , P.InstrAddq (P.ArgVar (P.Var "w")) (P.ArgVar (P.Var "z"))
    , P.InstrMovq (P.ArgVar (P.Var "y")) (P.ArgVar (P.Var "t.1"))
    , P.InstrNegq (P.ArgVar (P.Var "t.1"))
    , P.InstrMovq (P.ArgVar (P.Var "z")) (P.ArgReg P.RegRAX)
    , P.InstrAddq (P.ArgVar (P.Var "t.1")) (P.ArgReg P.RegRAX)
    , P.InstrJumpq (P.Label "conclusion")
    ]
  expected =
    [ P.InstrMovq (P.ArgInt 1) (P.ArgReg P.RegRCX)
    , P.InstrMovq (P.ArgInt 46) (P.ArgDeref P.RegRBP (-16))
    , P.InstrMovq (P.ArgReg P.RegRCX) (P.ArgReg P.RegRCX)
    , P.InstrAddq (P.ArgInt 7) (P.ArgDeref P.RegRBP (-16))
    , P.InstrMovq (P.ArgReg P.RegRCX) (P.ArgDeref P.RegRBP (-8))
    , P.InstrAddq (P.ArgInt 4) (P.ArgDeref P.RegRBP (-8))
    , P.InstrMovq (P.ArgReg P.RegRCX) (P.ArgReg P.RegRCX)
    , P.InstrAddq (P.ArgDeref P.RegRBP (-16)) (P.ArgReg P.RegRCX)
    , P.InstrMovq (P.ArgDeref P.RegRBP (-8)) (P.ArgDeref P.RegRBP (-8))
    , P.InstrNegq (P.ArgDeref P.RegRBP (-8))
    , P.InstrMovq (P.ArgReg P.RegRCX) (P.ArgReg P.RegRAX)
    , P.InstrAddq (P.ArgDeref P.RegRBP (-8)) (P.ArgReg P.RegRAX)
    , P.InstrJumpq (P.Label "conclusion")
    ]
