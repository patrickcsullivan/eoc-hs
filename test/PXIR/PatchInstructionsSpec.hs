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
      $ removeMovQSpec
    it "patches cmpq instruction if its second arg if it is not a register"
      $ cmpqRegArgSpec

noMultMemRefsSpec = patchInstructions input `shouldBe` input
 where
  input =
    [ P.InstrMovQ (P.ArgInt 10) (P.ArgDeref P.RegRBP (-8))
    , P.InstrNegQ (P.ArgDeref P.RegRBP (-8))
    , P.InstrMovQ (P.ArgInt 52) (P.ArgReg P.RegRAX)
    , P.InstrAddQ (P.ArgDeref P.RegRBP (-8)) (P.ArgReg P.RegRAX)
    , P.InstrJmp (P.Label "conclusion")
    ]

movqMultMemRefsSpec = patchInstructions input `shouldBe` expected
 where
  input =
    [ P.InstrMovQ (P.ArgInt 42) (P.ArgDeref P.RegRBP (-8))
    , P.InstrMovQ (P.ArgDeref P.RegRBP (-8)) (P.ArgDeref P.RegRBP (-16))
    , P.InstrMovQ (P.ArgDeref P.RegRBP (-16)) (P.ArgReg P.RegRAX)
    ]
  expected =
    [ P.InstrMovQ (P.ArgInt 42) (P.ArgDeref P.RegRBP (-8))
    , P.InstrMovQ (P.ArgDeref P.RegRBP (-8)) (P.ArgReg P.RegRAX)
    , P.InstrMovQ (P.ArgReg P.RegRAX) (P.ArgDeref P.RegRBP (-16))
    , P.InstrMovQ (P.ArgDeref P.RegRBP (-16)) (P.ArgReg P.RegRAX)
    ]

removeMovQSpec = patchInstructions input `shouldBe` expected
 where
  input =
    [ P.InstrMovQ (P.ArgDeref P.RegRBP (-8)) (P.ArgDeref P.RegRBP (-8))
    , P.InstrMovQ (P.ArgReg P.RegRCX) (P.ArgReg P.RegRCX)
    , P.InstrAddQ (P.ArgDeref P.RegRBP (-8)) (P.ArgReg P.RegRAX)
    , P.InstrJmp (P.Label "conclusion")
    ]
  expected =
    [ P.InstrAddQ (P.ArgDeref P.RegRBP (-8)) (P.ArgReg P.RegRAX)
    , P.InstrJmp (P.Label "conclusion")
    ]

cmpqRegArgSpec = patchInstructions input `shouldBe` expected
 where
  input =
    [ P.InstrCmpQ (P.ArgInt 0) (P.ArgDeref P.RegRBP (-8))
    , P.InstrJmpIf P.CCE (P.Label "block1")
    , P.InstrJmp (P.Label "block2")
    ]
  expected =
    [ P.InstrMovQ (P.ArgDeref P.RegRBP (-8)) (P.ArgReg P.RegRAX)
    , P.InstrCmpQ (P.ArgInt 0) (P.ArgReg P.RegRAX)
    , P.InstrJmpIf P.CCE (P.Label "block1")
    , P.InstrJmp (P.Label "block2")
    ]
