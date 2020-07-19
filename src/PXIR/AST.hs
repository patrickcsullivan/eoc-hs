{-|
Module      : PXIR.AST
Description : Pseudo-X86 Intermediate Representation
-}
module PXIR.AST where

import qualified Data.Map                      as M

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

newtype Label = Label { unlabel :: String } deriving (Eq, Ord)

data Instr
    = InstrAddq { src :: Arg, dst :: Arg }
    | InstrSubq { src :: Arg, dst :: Arg }
    | InstrMovq { src :: Arg, dst :: Arg }
    | InstrNegq Arg
    | InstrPushq Arg
    | InstrPopq Arg
    | InstrCallq Label
    | InstrJumpq Label
    | InstrRetq
    deriving Eq

data Block = Block
    { blockLabel :: Label
    , blockInstrs :: [Instr]
    } deriving Eq

data Program = Program { programBlocks :: M.Map Label Block } deriving Eq

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
    (InstrAddq src dst) -> "addq " ++ show src ++ ", " ++ show dst
    (InstrSubq src dst) -> "subq " ++ show src ++ ", " ++ show dst
    (InstrMovq src dst) -> "movq " ++ show src ++ ", " ++ show dst
    (InstrNegq  dst   ) -> "negq " ++ show dst
    (InstrPushq src   ) -> "pushq " ++ show src
    (InstrPopq  dst   ) -> "popq " ++ show dst
    (InstrCallq label ) -> "callq " ++ show label
    (InstrJumpq label ) -> "jmp " ++ show label
    InstrRetq           -> "retq"

instance Show Block where
  show (Block label instrs) = unlines (fmtLabel : fmtInstrs)
   where
    fmtLabel  = show label ++ ":"
    fmtInstrs = fmap (\instr -> "    " ++ show instr) instrs
