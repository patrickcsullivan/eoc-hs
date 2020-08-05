module PXIR.LivenessAnalysis.UncoverLiveSpec
  ( spec
  )
where

import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Set                      as S
import qualified PXIR.AST                      as P
import           PXIR.LivenessAnalysis.UncoverLive
import           Test.Hspec

spec :: Spec
spec = do
  describe "liveAfter" $ do
    it "returns sets of vars that are live after each instruction" $ basicSpec

basicSpec =
  let output = liveAfter (P.Label "start") inputBlocks
  in  do
        let outputStartBlock =
              fromMaybe (error "expected start block")
                $ M.lookup (P.Label "start") output
        map fst outputStartBlock `shouldBe` startInstrs
        map snd outputStartBlock `shouldBe` expectedStartLiveAfters
 where
  inputBlocks = M.singleton (P.Label "start") startInstrs
  startInstrs =
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
  expectedStartLiveAfters = map
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
