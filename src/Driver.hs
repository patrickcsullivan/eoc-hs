module Driver where

import qualified CIR.AST                       as C
import           CIR.SelectInstructions         ( selectInstructions )
import           CIR.UncoverVars                ( uncoverVars )
import qualified RIR.AST                       as R
import           RIR.UniquifyArgs               ( uniquifyArgs )
import           RIR.SimplifyArgs               ( simplifyArgs )
import           RIR.ExplicateControl           ( explicateControl )
import qualified PXIR.AST                      as P
import           PXIR.AssignHomes               ( assignHomesInBlock )
import           PXIR.PatchInstructions         ( patchInstructions )

drive :: R.Term -> String
drive rTrm =
  let (rTrm' , nextVar)     = uniquifyArgs rTrm 0
      (rTrm'', _      )     = simplifyArgs rTrm' nextVar
      cTail                 = explicateControl rTrm''
      localVars             = uncoverVars cTail
      pInstrs               = selectInstructions cTail
      pBlock                = P.Block (P.Label "start") pInstrs
      (pBlock', stackSpace) = assignHomesInBlock pBlock
      stackSpace'           = adjustStackSpace stackSpace
      pBlock''              = patchInstructions pBlock'
      main                  = mainBlock stackSpace' (P.Label "start")
      conclusion            = conclusionBlock stackSpace'
  in  writeBlocks main conclusion pBlock''

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

