module RIR.Parser
  ( parse
  )
where

import           Control.Monad
import qualified RIR.AST                       as R
import           RIR.Parser.STerm              as S
import           RIR.Parser.Transform

parse :: String -> Either String R.Term
parse s = do
  sTrm <- mapLeft show $ S.readTerm s
  transform sTrm

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f either = case either of
  Left  a -> Left (f a)
  Right b -> Right b
