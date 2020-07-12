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
    deriving (Show, Eq)

newtype Var = Var { unvar :: String } deriving (Show, Eq, Ord)

data Arg
    = ArgInt Int
    | ArgReg Reg
    | ArgDeref Reg Int
    | ArgVar Var
    deriving (Show, Eq)

newtype Label = Label { unlabel :: String } deriving (Show, Eq, Ord)

data Instr
    = InstrAddq { src :: Arg, dst :: Arg }
    | InstrSubq { src :: Arg, dst :: Arg }
    | InstrMovq { src :: Arg, dst :: Arg }
    | InstrNegq Arg
    | InstrPushq Arg
    | InstrPopq Arg
    | InstrCallq Label
    | InstrJumpq Label
    deriving (Show, Eq)

data Block = Block { blockInstrs :: [Instr] } deriving (Show, Eq)

data Program = Program { programBlocks :: M.Map Label Block } deriving (Show, Eq)
