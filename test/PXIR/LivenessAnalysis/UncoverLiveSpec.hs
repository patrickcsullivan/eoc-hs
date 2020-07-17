module PXIR.LivenessAnalysis.UncoverLiveSpec
  ( spec
  )
where

import qualified Data.Set                      as S
import qualified PXIR.AST                      as P
import           PXIR.LivenessAnalysis.UncoverLive
import           Test.Hspec

spec :: Spec
spec = do
  describe "liveAfterEach" $ do
    it "returns sets of vars that are live after each instruction" $ basicSpec

basicSpec =
  let output = liveAfterEach inputInstrs
  in  do
        map fst output `shouldBe` inputInstrs
        map snd output `shouldBe` expectedLiveAfters
 where
  inputInstrs =
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
  expectedLiveAfters = map
    varSetFromNames
    [ ["v"]
    , ["v", "w"]
    , ["w", "x"]
    , ["w", "x"]
    , ["w", "x", "y"]
    , ["w", "x", "y"]
    , ["w", "y", "z"]
    , ["y", "z"]
    , ["z", "t.1"]
    , ["z", "t.1"]
    , ["t.1"]
    , []
    , []
    ]

varSetFromNames :: [String] -> S.Set P.Var
varSetFromNames = S.fromList . map P.Var
