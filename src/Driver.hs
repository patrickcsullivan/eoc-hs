module Driver where

import qualified CIR.AST                       as C
import           CIR.SelectInstructions         ( selectInstructions )
import           CIR.UncoverVars                ( uncoverVars )
import qualified RIR.AST                       as R
import           RIR.Shrink                     ( shrink )
import qualified SRIR.AST                      as S
import           SRIR.UniquifyArgs              ( uniquifyArgs )
import           SRIR.SimplifyArgs              ( simplifyArgs )
import           SRIR.ExplicateControl          ( explicateControl )
import qualified PXIR.AST                      as P
import           PXIR.AssignHomes               ( assignHomes )
import           PXIR.PatchInstructions         ( patchInstructions )

drive :: R.Term -> String
drive rTrm =
  let sTrm                   = shrink rTrm
      (sTrm' , nextVar)      = uniquifyArgs sTrm 0
      (sTrm'', _      )      = simplifyArgs sTrm' nextVar
      cTail                  = explicateControl sTrm''
      localVars              = uncoverVars cTail
      pInstrs                = selectInstructions cTail
      (pInstrs', stackSpace) = assignHomes availableRegs pInstrs
      pInstrs''              = patchInstructions pInstrs'
      pBlock                 = P.Block (P.Label "start") pInstrs''
      stackSpace'            = adjustStackSpace stackSpace
      main                   = mainBlock stackSpace' (P.Label "start")
      conclusion             = conclusionBlock stackSpace'
  in  writeBlocks main conclusion pBlock

availableRegs :: [P.Reg]
availableRegs =
  [P.RegRDX, P.RegRCX, P.RegRSI, P.RegRDI, P.RegR8, P.RegR9, P.RegR10, P.RegR11]

writeBlocks :: P.Block -> P.Block -> P.Block -> String
writeBlocks main conclusion prog =
  show prog ++ "\n" ++ "    .globl main\n" ++ show main ++ show conclusion

adjustStackSpace :: Int -> Int
adjustStackSpace stackSpace =
  if stackSpace `mod` 16 == 0 then stackSpace else stackSpace + 8

mainBlock :: Int -> P.Label -> P.Block
mainBlock stackSpace jumpTo = P.Block (P.Label "main") instrs
 where
  instrs =
    [ P.InstrPushq (P.ArgReg P.RegRBP)
    , P.InstrMovq (P.ArgReg P.RegRSP) (P.ArgReg P.RegRBP)
    , P.InstrSubq (P.ArgInt stackSpace) (P.ArgReg P.RegRSP)
    , P.InstrJumpq jumpTo
    ]

conclusionBlock :: Int -> P.Block
conclusionBlock stackSpace = P.Block (P.Label "conclusion") instrs
 where
  instrs =
    [ P.InstrAddq (P.ArgInt stackSpace) (P.ArgReg P.RegRSP)
    , P.InstrPopq (P.ArgReg P.RegRBP)
    , P.InstrRetq
    ]
