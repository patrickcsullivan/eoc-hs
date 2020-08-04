module PXIR.LivenessAnalysis.ControlFlowGraphSpec
  ( spec
  )
where

import qualified Algebra.Graph                 as G
import qualified Data.Set                      as S
import qualified PXIR.AST                      as P
import qualified PXIR.LivenessAnalysis.ControlFlowGraph
                                               as CG
import           Test.Hspec

spec :: Spec
spec = do
  describe "make" $ do
    it "builds graph" $ makeSpec
  describe "topoSort" $ do
    it "topolgically sorts graph" $ topoSortSpec

makeSpec = do
  G.vertexList output `shouldBe` G.vertexList expectedGraph
  G.edgeList output `shouldBe` G.edgeList expectedGraph
 where
  output = CG.make input
  input =
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
  expectedGraph =
    let
      start  = P.Label "start"
      block0 = P.Label "block0"
      block1 = P.Label "block1"
      block2 = P.Label "block2"
      vs     = G.vertices [start, block0, block1, block2]
      es     = G.edges
        [(start, block1), (start, block2), (block1, block0), (block2, block0)]
    in
      G.overlay vs es

topoSortSpec = CG.topoSort (P.Label "start") inputGraph `shouldBe` expected
 where
  start  = P.Label "start"
  block0 = P.Label "block0"
  block1 = P.Label "block1"
  block2 = P.Label "block2"
  inputGraph =
    let vs = G.vertices [start, block0, block1, block2]
        es =
            G.edges
              [ (start , block1)
              , (start , block2)
              , (block1, block0)
              , (block2, block0)
              ]
    in  G.overlay vs es
  expected = [start, block1, block2, block0]
