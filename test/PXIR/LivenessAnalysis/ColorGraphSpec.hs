module PXIR.LivenessAnalysis.ColorGraphSpec
  ( spec
  )
where

import qualified Algebra.Graph                 as G
import qualified Data.Map                      as M
import           Data.Maybe                     ( mapMaybe )
import qualified Data.Set                      as S
import qualified PXIR.AST                      as P
import           PXIR.LivenessAnalysis.ColorGraph
                                                ( colorGraph )
import qualified PXIR.LivenessAnalysis.ConflictGraph
                                               as CG
import           Test.Hspec

spec :: Spec
spec = do
  describe "colorGraph" $ do
    describe "coloring basic example conflict graph" basicSpec

basicSpec = do
  it "no adjacent vars have the same color"
    $          hasAdjacentMatches inputGraph output
    `shouldBe` False
  it "uses 3 colors" $ colorCount output `shouldBe` 3
 where
  output = colorGraph inputGraph
  inputGraph =
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

hasAdjacentMatches :: CG.ConflictGraph -> M.Map P.Var Int -> Bool
hasAdjacentMatches cg varToColor =
  any colorsMatch $ mapMaybe edgeColors $ G.edgeList cg
 where
  edgeColors (vert1, vert2) = do
    var1   <- CG.maybeVar vert1
    var2   <- CG.maybeVar vert2
    color1 <- M.lookup var1 varToColor
    color2 <- M.lookup var2 varToColor
    return (color1, color2)
  colorsMatch (color1, color2) = color1 == color2

colorCount :: M.Map P.Var Int -> Int
colorCount varToColor =
  if M.null varToColor then 0 else (maximum $ M.elems varToColor) + 1
