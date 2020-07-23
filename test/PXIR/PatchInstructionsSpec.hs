module PXIR.PatchInstructionsSpec
  ( spec
  )
where

import qualified PXIR.AST                      as P
import           PXIR.PatchInstructions
import           Test.Hspec

spec :: Spec
spec = do
  describe "patchInstructions" $ do
    it "when no instructions have multiple memory refs it doesn't change block"
      $ noMultMemRefsSpec
    it "when movq instruction has multiple memory refs it patches instructions"
      $ movqMultMemRefsSpec
    it "removes movq instructions that have matching src and dst args"
      $ removeMovqSpec

noMultMemRefsSpec = patchInstructions input `shouldBe` input
 where
  input =
    [ P.InstrMovq (P.ArgInt 10) (P.ArgDeref P.RegRBP (-8))
    , P.InstrNegq (P.ArgDeref P.RegRBP (-8))
    , P.InstrMovq (P.ArgInt 52) (P.ArgReg P.RegRAX)
    , P.InstrAddq (P.ArgDeref P.RegRBP (-8)) (P.ArgReg P.RegRAX)
    , P.InstrJumpq (P.Label "conclusion")
    ]

movqMultMemRefsSpec = patchInstructions input `shouldBe` expected
 where
  input =
    [ P.InstrMovq (P.ArgInt 42) (P.ArgDeref P.RegRBP (-8))
    , P.InstrMovq (P.ArgDeref P.RegRBP (-8)) (P.ArgDeref P.RegRBP (-16))
    , P.InstrMovq (P.ArgDeref P.RegRBP (-16)) (P.ArgReg P.RegRAX)
    ]
  expected =
    [ P.InstrMovq (P.ArgInt 42) (P.ArgDeref P.RegRBP (-8))
    , P.InstrMovq (P.ArgDeref P.RegRBP (-8)) (P.ArgReg P.RegRAX)
    , P.InstrMovq (P.ArgReg P.RegRAX) (P.ArgDeref P.RegRBP (-16))
    , P.InstrMovq (P.ArgDeref P.RegRBP (-16)) (P.ArgReg P.RegRAX)
    ]

removeMovqSpec = patchInstructions input `shouldBe` expected
 where
  input =
    [ P.InstrMovq (P.ArgDeref P.RegRBP (-8)) (P.ArgDeref P.RegRBP (-8))
    , P.InstrMovq (P.ArgReg P.RegRCX) (P.ArgReg P.RegRCX)
    , P.InstrAddq (P.ArgDeref P.RegRBP (-8)) (P.ArgReg P.RegRAX)
    , P.InstrJumpq (P.Label "conclusion")
    ]
  expected =
    [ P.InstrAddq (P.ArgDeref P.RegRBP (-8)) (P.ArgReg P.RegRAX)
    , P.InstrJumpq (P.Label "conclusion")
    ]
