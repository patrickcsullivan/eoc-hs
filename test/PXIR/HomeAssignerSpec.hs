module PXIR.HomeAssignerSpec
  ( spec
  )
where

import qualified PXIR.AST                      as P
import           PXIR.HomeAssigner
import           Test.Hspec

spec :: Spec
spec = do
  describe "assignHomesInBlock" $ do
    it "assigns vars to stack in basic add and neg example" $ basicAddAndNegSpec
    it "assigns vars to stack when move has src and dst var args"
      $ moveWithSrcDstVars

basicAddAndNegSpec =
  assignHomesInBlock inputBlock `shouldBe` (expectedBlock, 8)
 where
  inputBlock =
    (P.Block
      [ P.InstrMovq (P.ArgInt 10) (P.ArgVar (P.Var "_0"))
      , P.InstrNegq (P.ArgVar (P.Var "_0"))
      , P.InstrMovq (P.ArgInt 52) (P.ArgReg P.RegRAX)
      , P.InstrAddq (P.ArgVar (P.Var "_0")) (P.ArgReg P.RegRAX)
      , P.InstrJumpq (P.Label "conclusion")
      ]
    )
  expectedBlock =
    (P.Block
      [ P.InstrMovq (P.ArgInt 10) (P.ArgDeref P.RegRBP (-8))
      , P.InstrNegq (P.ArgDeref P.RegRBP (-8))
      , P.InstrMovq (P.ArgInt 52) (P.ArgReg P.RegRAX)
      , P.InstrAddq (P.ArgDeref P.RegRBP (-8)) (P.ArgReg P.RegRAX)
      , P.InstrJumpq (P.Label "conclusion")
      ]
    )

moveWithSrcDstVars =
  assignHomesInBlock inputBlock `shouldBe` (expectedBlock, 16)
 where
  inputBlock =
    (P.Block
      [ P.InstrMovq (P.ArgInt 10) (P.ArgVar (P.Var "x.1"))
      , P.InstrMovq (P.ArgVar (P.Var "x.1")) (P.ArgVar (P.Var "x.2"))
      , P.InstrMovq (P.ArgVar (P.Var "x.2")) (P.ArgReg P.RegRAX)
      , P.InstrJumpq (P.Label "conclusion")
      ]
    )
  expectedBlock =
    (P.Block
      [ P.InstrMovq (P.ArgInt 10) (P.ArgDeref P.RegRBP (-8))
      , P.InstrMovq (P.ArgDeref P.RegRBP (-8)) (P.ArgDeref P.RegRBP (-16))
      , P.InstrMovq (P.ArgDeref P.RegRBP (-16)) (P.ArgReg P.RegRAX)
      , P.InstrJumpq (P.Label "conclusion")
      ]
    )
