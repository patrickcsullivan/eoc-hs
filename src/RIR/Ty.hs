module RIR.Ty where

import qualified Data.Map                      as M
import           RIR.AST

data Ty
  = TyBool
  | TyInt

instance Show Ty where
  show ty = case ty of
    TyBool -> "Bool"
    TyInt  -> "Int"

{- | Typing context that maps variable names to types.
-}
type Ctx = M.Map Var Ty

-- typeCheck :: Ctx -> Term -> Ty
-- typeCheck ctx trm = case trm of
--     TermRead -> TyInt
