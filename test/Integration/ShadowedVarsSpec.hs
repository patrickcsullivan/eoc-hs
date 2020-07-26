module Integration.ShadowedVarsSpec
  ( spec
  )
where

import           Driver                         ( drive )
import qualified SRIR.AST                      as S
import           Test.Hspec

spec :: Spec
spec = do
  describe "drive" $ do
    it "compiles shadowed vars example" $ drive input `shouldBe` expected

input :: S.Term
input = S.TermLet
  (S.Var "my_var")
  (S.TermInt 42)
  (S.TermLet
    (S.Var "input")
    S.TermRead
    (S.TermLet
      (S.Var "my_var")
      (S.TermAdd (S.TermVar (S.Var "my_var"))
                 (S.TermNeg (S.TermVar (S.Var "input")))
      )
      (S.TermVar (S.Var "my_var"))
    )
  )

expected :: String
expected =
  "start:\n\
    \    movq $42, %rcx\n\
    \    callq read_int\n\
    \    movq %rax, %rdx\n\
    \    negq %rdx\n\
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
