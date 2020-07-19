module PXIR.LivenessAnalysis.ConflictGraphSpec
  ( spec
  )
where

import qualified Algebra.Graph                 as G
import qualified Data.Set                      as S
import qualified PXIR.AST                      as P
import           PXIR.LivenessAnalysis.UncoverLive
                                                ( liveAfterEach )
import qualified PXIR.LivenessAnalysis.ConflictGraph
                                               as CG
import           Test.Hspec

spec :: Spec
spec = do
  describe "make" $ do
    it "builds graph from the output of `liveAfterEach` for a simple example"
      $ basicSpec
    it "builds graph with dst var verticies but no conflict edges"
      $ noConflictsSpec
    describe "for addq instructions" $ do
      it "connects dst var with each live-after var except the dst var"
        $ addqSpec
      it "adds dst var but no edges when instr has no live-after vars"
        $ addqNoExtraLiveAfterSpec
      it "doesn't add any vertices or edges when dst arg is not a var"
        $ addqNoDstVar
    describe "for movq instructions" $ do
      it "connects dst var with each live-after var except the src and dst vars"
        $ movqSpec
      it "adds dst var but no edges when instr has no live-after vars"
        $ movqNoExtraLiveAfterSpec
      it "doesn't add any vertices or edges when dst arg is not a var"
        $ movqNoDstVar

basicSpec = do
  G.vertexList output `shouldBe` G.vertexList expectedGraph
  G.edgeList output `shouldBe` G.edgeList expectedGraph
 where
  output = CG.make (liveAfterEach inputInstrs)
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
  expectedGraph =
    let v  = Left (P.Var "v")
        w  = Left (P.Var "w")
        x  = Left (P.Var "x")
        y  = Left (P.Var "y")
        z  = Left (P.Var "z")
        t1 = Left (P.Var "t.1")
        vs = G.vertices [v, w, x, y, z, t1]
        es = G.edges
          [ (v , w)
          , (w , v)
          , (w , x)
          , (w , y)
          , (w , z)
          , (x , w)
          , (x , y)
          , (y , w)
          , (y , x)
          , (y , z)
          , (z , w)
          , (z , y)
          , (z , t1)
          , (t1, z)
          ]
    in  G.overlay vs es

noConflictsSpec = do
  G.vertexList output `shouldBe` G.vertexList expectedGraph
  G.edgeList output `shouldBe` G.edgeList expectedGraph
 where
  output = CG.make (liveAfterEach inputInstrs)
  inputInstrs =
    [ P.InstrMovq (P.ArgInt 1) (P.ArgVar (P.Var "x"))
    , P.InstrMovq (P.ArgInt 46) (P.ArgVar (P.Var "y"))
    , P.InstrJumpq (P.Label "conclusion")
    ]
  expectedGraph =
    let x = Left (P.Var "x")
        y = Left (P.Var "y")
    in  G.vertices [x, y]

addqSpec = do
  G.vertexList output `shouldBe` G.vertexList expectedGraph
  G.edgeList output `shouldBe` G.edgeList expectedGraph
 where
  output =
    CG.make
      $ [ ( P.InstrAddq (P.ArgVar (P.Var "w")) (P.ArgVar (P.Var "x"))
          , S.fromList [(P.Var "w"), (P.Var "x"), (P.Var "y"), (P.Var "z")]
          )
        ]
  expectedGraph =
    let w  = Left (P.Var "w")
        x  = Left (P.Var "x")
        y  = Left (P.Var "y")
        z  = Left (P.Var "z")
        vs = G.vertices [w, x, y, z]
        es = G.edges [(w, x), (x, w), (x, y), (x, z), (y, x), (z, x)]
    in  G.overlay vs es

addqNoExtraLiveAfterSpec = do
  G.vertexList output `shouldBe` G.vertexList expectedGraph
  G.edgeList output `shouldBe` G.edgeList expectedGraph
 where
  output =
    CG.make $ [(P.InstrAddq (P.ArgInt 1) (P.ArgVar (P.Var "x")), S.empty)]
  expectedGraph = G.vertices [Left (P.Var "x")]

addqNoDstVar = do
  G.vertexList output `shouldBe` G.vertexList expectedGraph
  G.edgeList output `shouldBe` G.edgeList expectedGraph
 where
  output =
    CG.make
      $ [ ( P.InstrAddq (P.ArgInt 1) (P.ArgReg P.RegRAX)
          , S.fromList [(P.Var "x"), (P.Var "y"), (P.Var "z")]
          )
        ]
  expectedGraph = G.empty

movqSpec = do
  G.vertexList output `shouldBe` G.vertexList expectedGraph
  G.edgeList output `shouldBe` G.edgeList expectedGraph
 where
  output =
    CG.make
      $ [ ( P.InstrMovq (P.ArgVar (P.Var "w")) (P.ArgVar (P.Var "x"))
          , S.fromList [(P.Var "w"), (P.Var "x"), (P.Var "y"), (P.Var "z")]
          )
        ]
  expectedGraph =
    let w  = Left (P.Var "w")
        x  = Left (P.Var "x")
        y  = Left (P.Var "y")
        z  = Left (P.Var "z")
        -- Doesn't include dst since w isnn't the dst or one of the live-after vars that will be connected.
        vs = G.vertices [x, y, z]
        es = G.edges [(x, y), (x, z), (y, x), (z, x)]
    in  G.overlay vs es

movqNoExtraLiveAfterSpec = do
  G.vertexList output `shouldBe` G.vertexList expectedGraph
  G.edgeList output `shouldBe` G.edgeList expectedGraph
 where
  output =
    CG.make $ [(P.InstrMovq (P.ArgInt 1) (P.ArgVar (P.Var "x")), S.empty)]
  expectedGraph = G.vertices [Left (P.Var "x")]

movqNoDstVar = do
  G.vertexList output `shouldBe` G.vertexList expectedGraph
  G.edgeList output `shouldBe` G.edgeList expectedGraph
 where
  output =
    CG.make
      $ [ ( P.InstrMovq (P.ArgInt 1) (P.ArgReg P.RegRAX)
          , S.fromList [(P.Var "x"), (P.Var "y"), (P.Var "z")]
          )
        ]
  expectedGraph = G.empty
