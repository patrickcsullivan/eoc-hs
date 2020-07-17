module Integration.ShadowedVarsSpec
  ( spec
  )
where

import           Driver                         ( drive )
import qualified RIR.AST                       as R
import           Test.Hspec

spec :: Spec
spec = do
  describe "drive" $ do
    it "compiles shadowed vars example" $ drive input `shouldBe` expected

input :: R.Term
input = R.TermLet
  (R.Var "my_var")
  (R.TermVal (R.ValueInt 42))
  (R.TermLet
    (R.Var "input")
    R.TermRead
    (R.TermLet
      (R.Var "my_var")
      (R.TermAdd (R.TermVar (R.Var "my_var"))
                 (R.TermNeg (R.TermVar (R.Var "input")))
      )
      (R.TermVar (R.Var "my_var"))
    )
  )

expected :: String
expected =
  "start:\n\
    \    movq $42, -8(%rbp)\n\
    \    callq read_int\n\
    \    movq %rax, -16(%rbp)\n\
    \    movq -16(%rbp), %rax\n\
    \    movq %rax, -24(%rbp)\n\
    \    negq -24(%rbp)\n\
    \    movq -8(%rbp), %rax\n\
    \    movq %rax, -32(%rbp)\n\
    \    movq -24(%rbp), %rax\n\
    \    addq %rax, -32(%rbp)\n\
    \    movq -32(%rbp), %rax\n\
    \    jmp conclusion\n\
    \\n\
    \    .globl main\n\
    \main:\n\
    \    pushq %rbp\n\
    \    movq %rsp, %rbp\n\
    \    subq $32, %rsp\n\
    \    jmp start\n\
    \conclusion:\n\
    \    addq $32, %rsp\n\
    \    popq %rbp\n\
    \    retq\n\
    \"
