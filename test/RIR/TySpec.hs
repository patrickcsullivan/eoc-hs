module RIR.TySpec
  ( spec
  )
where

import           Data.Either                    ( isLeft )
import           RIR.AST
import           RIR.Ty
import           Test.Hspec

spec :: Spec
spec = do
  describe "typeOf" $ do
    it "typechecks read" $ readSpec
    it "typechecks comparison operators" $ comparisonSpec
    it "typechecks logical operators" $ logicalSpec
    it "typechecks arithmetic operators" $ arithmeticSpec
    it "typechecks if expression with Int arms" $ ifIntSpec
    it "typechecks if expression with Bool arms" $ ifBoolSpec
    it "typechecks let and var" $ letVarSpec
    it "fails for comparison arg that isn't Int" $ failComparisonSpec
    it "fails for logical arg that isn't Bool" $ failLogicalSpec
    it "fails for arithmetic arg that isn't Int" $ failArithmeticSpec
    it "fails for if arms that don't match" $ failIfArmsSpec
    it "fails for if condition that isn't Bool" $ failIfConditionSpec
    it "fails for var that is not in context" $ failVarSpec

readSpec = typeOf emptyCtx input `shouldBe` Right TyInt where input = TermRead

comparisonSpec = typeOf emptyCtx input `shouldBe` Right TyBool
  where input = TermEq (TermInt 5) (TermInt 7)

logicalSpec = typeOf emptyCtx input `shouldBe` Right TyBool
 where
  input =
    TermAnd (TermNot (TermBool False)) (TermOr (TermBool True) (TermBool False))

arithmeticSpec = typeOf emptyCtx input `shouldBe` Right TyInt
  where input = TermAdd (TermNeg (TermInt 1)) (TermSub (TermInt 7) (TermInt 2))

ifIntSpec = typeOf emptyCtx input `shouldBe` Right TyInt
  where input = TermIf (TermBool False) (TermInt 5) (TermInt 3)

ifBoolSpec = typeOf emptyCtx input `shouldBe` Right TyBool
  where input = TermIf (TermBool False) (TermBool True) (TermBool False)

letVarSpec = typeOf emptyCtx input `shouldBe` Right TyInt
  where input = TermLet (Var "x") (TermInt 5) (TermVar (Var "x"))

failComparisonSpec = typeOf emptyCtx input `shouldSatisfy` isLeft
  where input = TermEq (TermBool True) (TermInt 7)

failLogicalSpec = typeOf emptyCtx input `shouldSatisfy` isLeft
  where input = TermAnd (TermBool False) (TermInt 1)

failArithmeticSpec = typeOf emptyCtx input `shouldSatisfy` isLeft
  where input = TermAdd (TermBool True) (TermInt 7)

failIfArmsSpec = typeOf emptyCtx input `shouldSatisfy` isLeft
  where input = TermIf (TermBool True) (TermInt 7) (TermBool False)

failIfConditionSpec = typeOf emptyCtx input `shouldSatisfy` isLeft
  where input = TermIf (TermInt 0) (TermInt 7) (TermInt 5)

failVarSpec = typeOf emptyCtx input `shouldSatisfy` isLeft
  where input = TermVar (Var "x")
