{-|
Module      : PXIR.AST
Description : Pseudo-X86 Intermediate Representation
-}
module PXIR.AST where

import qualified Data.Map                      as M

data ByteReg
    = ByteRegAL
    deriving Eq

data Reg
    = RegRSP
    | RegRBP
    | RegRAX
    | RegRBX
    | RegRCX
    | RegRDX
    | RegRSI
    | RegRDI
    | RegR8
    | RegR9
    | RegR10
    | RegR11
    | RegR12
    | RegR13
    | RegR14
    | RegR15
    deriving (Eq, Ord)

newtype Var = Var { unvar :: String } deriving (Eq, Ord)

data Arg
    = ArgInt Int
    | ArgReg Reg
    | ArgDeref Reg Int
    | ArgVar Var
    deriving Eq

{- | Condition code.
-}
data CC
  = CCE -- | Equal.
  | CCL -- | Less than.
  | CCLE -- | Less than or equal.
  | CCG -- | Greater than.
  | CCGE -- | Greater than or equal.
  deriving Eq

newtype Label = Label { unlabel :: String } deriving (Eq, Ord)

data Instr
    = InstrAddQ { src :: Arg, dst :: Arg }
    | InstrSubQ { src :: Arg, dst :: Arg }
    | InstrMovQ { src :: Arg, dst :: Arg }
    | InstrMovZBQ { bsrc :: ByteReg, dst :: Arg }
    | InstrNegQ Arg
    | InstrPushQ Arg
    | InstrPopQ Arg
    | InstrCallQ Label
    | InstrRetQ
    | InstrXOrQ { src :: Arg, dst :: Arg }
    | InstrCmpQ { src2 :: Arg, src1 :: Arg }
    | InstrSet { cc :: CC, bdst :: ByteReg }
    | InstrJmp Label
    | InstrJmpIf CC Label
    | InstrLabel Label
    deriving Eq

data Block = Block
    { blockLabel :: Label
    , blockInstrs :: [Instr]
    } deriving Eq

data Program = Program { programBlocks :: M.Map Label Block } deriving Eq

instance Show ByteReg where
  show breg = case breg of
    ByteRegAL -> "al"

instance Show Reg where
  show reg = case reg of
    RegRSP -> "rsp"
    RegRBP -> "rbp"
    RegRAX -> "rax"
    RegRBX -> "rbx"
    RegRCX -> "rcx"
    RegRDX -> "rdx"
    RegRSI -> "rsi"
    RegRDI -> "rdi"
    RegR8  -> "r8"
    RegR9  -> "r9"
    RegR10 -> "r10"
    RegR11 -> "r11"
    RegR12 -> "r12"
    RegR13 -> "r13"
    RegR14 -> "r14"
    RegR15 -> "r15"

instance Show Var where
  show (Var name) = name

instance Show Arg where
  show arg = case arg of
    ArgInt n       -> "$" ++ show n
    ArgReg r       -> "%" ++ show r
    ArgDeref r off -> show off ++ "(%" ++ show r ++ ")"
    ArgVar v       -> "<" ++ show v ++ ">"

instance Show Label where
  show (Label name) = name

instance Show Instr where
  show instr = case instr of
    (InstrAddQ src dst) -> "addq " ++ show src ++ ", " ++ show dst
    (InstrSubQ src dst) -> "subq " ++ show src ++ ", " ++ show dst
    (InstrMovQ src dst) -> "movq " ++ show src ++ ", " ++ show dst
    (InstrMovZBQ bsrc dst) ->
      "movzbq" ++ "(%" ++ show bsrc ++ "), " ++ show dst
    (InstrNegQ  dst  )      -> "negq " ++ show dst
    (InstrPushQ src  )      -> "pushq " ++ show src
    (InstrPopQ  dst  )      -> "popq " ++ show dst
    (InstrCallQ label)      -> "callq " ++ show label
    InstrRetQ               -> "retq"
    (InstrXOrQ src  dst   ) -> "xorq " ++ show src ++ ", " ++ show dst
    (InstrCmpQ src2 src1  ) -> "cmpq " ++ show src2 ++ ", " ++ show src1
    (InstrSet  CCE  dst   ) -> "sete" ++ show dst
    (InstrSet  CCL  dst   ) -> "setl" ++ show dst
    (InstrSet  CCLE dst   ) -> "setle" ++ show dst
    (InstrSet  CCG  dst   ) -> "setg" ++ show dst
    (InstrSet  CCGE dst   ) -> "setge" ++ show dst
    (InstrJmp label       ) -> "jmp " ++ show label
    (InstrJmpIf CCE  label) -> "je" ++ show label
    (InstrJmpIf CCL  label) -> "jl" ++ show label
    (InstrJmpIf CCLE label) -> "jle" ++ show label
    (InstrJmpIf CCG  label) -> "jg" ++ show label
    (InstrJmpIf CCGE label) -> "jge" ++ show label
    InstrLabel label        -> show label ++ ":"

instance Show Block where
  show (Block label instrs) = unlines (fmtLabel : fmtInstrs)
   where
    fmtLabel  = show label ++ ":"
    fmtInstrs = fmap (\instr -> "    " ++ show instr) instrs
