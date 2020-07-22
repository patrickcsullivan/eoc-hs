module PXIR.LivenessAnalysis.ColorGraph
  ( colorGraph
  )
where

import qualified Algebra.Graph                 as G
import           Control.Monad.State
import qualified Data.List                     as L
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                )
import qualified Data.Set                      as S
import           PXIR.AST
import           PXIR.LivenessAnalysis.ConflictGraph
                                                ( ConflictGraph
                                                , ConflictVertex
                                                , maybeVar
                                                )

{- | Context that maintains the state necessary for coloring a conflict graph.
-}
data Ctx = Ctx
    {- | Maps each variable to a set of colors, represented by natural numbers,
    that are unavailable to the variable. Contains only variables that have not
    been colored yet.
    -}
    { ctxVarToUnavail :: !(M.Map Var (S.Set Int))

    {- | Maps each variable name to its color, represented by a natural number..
    -}
    , ctxVarToColor :: !(M.Map Var Int)
    }

{- | Create a new context where all variables are initially uncolored and all
variables have no unavailable colors.
-}
newCtx :: S.Set Var -> Ctx
newCtx vars = Ctx { ctxVarToUnavail = M.fromSet (\_ -> S.empty) vars
                  , ctxVarToColor   = M.empty
                  }

{- | State monad that wraps the context.
-}
type CtxS a = State Ctx a

{- | Find the neighbors of the given vertex. A vertex, v', is a neighbor of the
giver vertex, v, if there exists an edge from v to v'.
-}
neighborsOf :: ConflictGraph -> ConflictVertex -> [ConflictVertex]
neighborsOf cg v = mapMaybe maybeNeighbor $ G.edgeList cg
  where maybeNeighbor (src, dst) = if src == v then Just dst else Nothing

{- | Mark the given color as unavailable for the given vertex.
-}
markUnavailable :: Int -> Var -> CtxS ()
markUnavailable col var = do
  (Ctx varToUnavail varToColor) <- get
  let varToUnavail' = M.adjust (S.insert col) var varToUnavail
  put (Ctx varToUnavail' varToColor)
  return ()

{- | Mark the given color as unavailable for the neighbors of the specified
variable if those neighbors have not been colored yet.
-}
markUnavailableForNeighbors :: ConflictGraph -> Var -> Int -> CtxS ()
markUnavailableForNeighbors cg var col =
  let varNeighbors = mapMaybe maybeVar $ neighborsOf cg (Left var)
  in  do
        mapM (markUnavailable col) varNeighbors
        return ()

{- | Delete the variable from the mapping of variables to unavailable colors.
-}
deleteFromVarToUnavail :: Var -> CtxS ()
deleteFromVarToUnavail var = do
  (Ctx varToUnavail varToColor) <- get
  let varToUnavail' = M.delete var varToUnavail
  put (Ctx varToUnavail' varToColor)
  return ()

{- | Insert the variable and color into the mapping of variables to assigned
colors.
-}
insertIntoVarToColor :: Var -> Int -> CtxS ()
insertIntoVarToColor var col = do
  (Ctx varToUnavail varToColor) <- get
  let varToColor' = M.insert var col varToColor
  put (Ctx varToUnavail varToColor')
  return ()

{- | Assign a variable in the graph to the given color, represented by a natural
number. Remove the variable from the mapping of variables to unavailable colors
and add it to the mapping of viariables to assigned colors. Mark the assigned
color as unavailable for all of the variable's neighbors in the conflict graph.
-}
assignColor :: ConflictGraph -> Var -> Int -> CtxS ()
assignColor cg var col = do
  deleteFromVarToUnavail var
  insertIntoVarToColor var col
  markUnavailableForNeighbors cg var col
  return ()

{- | Find the variable with the highest saturation of unavailable colors.
-}
maxSaturation :: M.Map Var (S.Set Int) -> Maybe (Var, S.Set Int)
maxSaturation varToUnavail = if M.null varToUnavail
  then Nothing
  else Just $ L.maximumBy compareSat $ M.toList varToUnavail
 where
  compareSat (_, unavail1) (_, unavail2) =
    L.length unavail1 `compare` L.length unavail2

{- | Given a set of natural numbers, find the smallest natural number that is
not in the set.
-}
nextSmallest :: S.Set Int -> Int
nextSmallest ns = fromMaybe (S.size ns) $ L.find match idxs
 where
  idxs = [0 .. S.size ns - 1]
  match i | i == S.toAscList ns !! i = False
          | otherwise                = True

{- | Assign a color, represented by a natural number, to each vertex in the
graph. No adjacent verticies will have the same color.
-}
colorGraphS :: ConflictGraph -> CtxS (M.Map Var Int)
colorGraphS cg = do
  (Ctx varToUnavail varToColor) <- get
  case maxSaturation varToUnavail of
    Nothing                   -> return varToColor
    Just (var, unavailColors) -> do
      let color = nextSmallest unavailColors
      assignColor cg var color
      colorGraphS cg

{- | Assign a color, represented by a natural number, to each vertex in the
graph. No adjacent verticies will have the same color.
-}
colorGraph :: ConflictGraph -> M.Map Var Int
colorGraph cg = fst $ runState (colorGraphS cg) (newCtx vars)
 where
  vars = S.fromList $ mapMaybe maybeVar $ G.vertexList cg
  maybeVar vert = case vert of
    Left var -> Just var
    _        -> Nothing
