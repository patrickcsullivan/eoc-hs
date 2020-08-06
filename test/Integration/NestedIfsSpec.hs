module Integration.NestedIfsSpec
  ( spec
  )
where

import           Driver                         ( drive )
import qualified RIR.AST                       as R
import           Test.Hspec

spec :: Spec
spec = do
  describe "drive" $ do
    it "compiles nested ifs example" $ drive input `shouldBe` expected

input :: R.Term
input = R.TermIf
  (R.TermIf (R.TermEq R.TermRead (R.TermInt 1))
            (R.TermEq R.TermRead (R.TermInt 0))
            (R.TermEq R.TermRead (R.TermInt 2))
  )
  (R.TermAdd (R.TermInt 10) (R.TermInt 32))
  (R.TermSub (R.TermInt 700) (R.TermInt 77))

expected :: String
expected = ""
