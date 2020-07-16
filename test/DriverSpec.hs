module DriverSpec
  ( spec
  )
where

import           Driver                         ( drive )
import qualified RIR.AST                       as R
import           Test.Hspec

spec :: Spec
spec = do
  describe "drive" $ do
    it "doesn't crash" $ basicSpec

basicSpec = drive input `shouldBe` ""
 where
  input = R.TermLet
    (R.Var "my_var")
    (R.TermVal (R.ValueInt 42))
    (R.TermLet
      (R.Var "input")
      R.TermRead
      (R.TermLet
        (R.Var "my_var")
        (R.TermAdd (R.TermVar (R.Var "my_value"))
                   (R.TermNeg (R.TermVar (R.Var "input")))
        )
        (R.TermVar (R.Var "my_value"))
      )
    )
