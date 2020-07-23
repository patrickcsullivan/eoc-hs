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
    \    movq $42, %rcx\n\
    \    callq read_int\n\
    \    movq %rax, %rdx\n\
    \    movq %rdx, %rdx\n\
    \    negq %rdx\n\
    \    movq %rcx, %rcx\n\
    \    addq %rdx, %rcx\n\
    \    movq %rcx, %rax\n\
    \    jmp conclusion\n\
    \\n\
    \    .globl main\n\
    \main:\n\
    \    pushq %rbp\n\
    \    movq %rsp, %rbp\n\
    \    subq $0, %rsp\n\
    \    jmp start\n\
    \conclusion:\n\
    \    addq $0, %rsp\n\
    \    popq %rbp\n\
    \    retq\n\
    \"
