module PXIR.InstrPatcherSpec
  ( spec
  )
where

import qualified PXIR.AST                      as P
import           PXIR.InstrPatcher
import           Test.Hspec

spec :: Spec
spec = do
  describe "patchInstructions" $ do
    it "when no instructions have multiple memory refs it doesn't change block"
      $ noMultMemRefsSpec
    it "when movq instruction has multiple memory refs it patches instructions"
      $ movqMultMemRefsSpec

noMultMemRefsSpec = patchInstructions inputBlock `shouldBe` inputBlock
 where
  inputBlock =
    (P.Block
      [ P.InstrMovq (P.ArgInt 10) (P.ArgDeref P.RegRBP (-8))
      , P.InstrNegq (P.ArgDeref P.RegRBP (-8))
      , P.InstrMovq (P.ArgInt 52) (P.ArgReg P.RegRAX)
      , P.InstrAddq (P.ArgDeref P.RegRBP (-8)) (P.ArgReg P.RegRAX)
      , P.InstrJumpq (P.Label "conclusion")
      ]
    )

movqMultMemRefsSpec = patchInstructions inputBlock `shouldBe` expectedBlock
 where
  inputBlock =
    (P.Block
      [ P.InstrMovq (P.ArgInt 42) (P.ArgDeref P.RegRBP (-8))
      , P.InstrMovq (P.ArgDeref P.RegRBP (-8)) (P.ArgDeref P.RegRBP (-16))
      , P.InstrMovq (P.ArgDeref P.RegRBP (-16)) (P.ArgReg P.RegRAX)
      ]
    )
  expectedBlock =
    (P.Block
      [ P.InstrMovq (P.ArgInt 42) (P.ArgDeref P.RegRBP (-8))
      , P.InstrMovq (P.ArgDeref P.RegRBP (-8)) (P.ArgReg P.RegRAX)
      , P.InstrMovq (P.ArgReg P.RegRAX) (P.ArgDeref P.RegRBP (-16))
      , P.InstrMovq (P.ArgDeref P.RegRBP (-16)) (P.ArgReg P.RegRAX)
      ]
    )
