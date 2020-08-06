module Driver where

import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
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

startBlockName :: String
startBlockName = "start"

drive :: R.Term -> String
drive rTrm =
  let sTrm               = shrink rTrm
      (sTrm'  , nextVar) = uniquifyArgs sTrm 0
      (sTrm'' , _      ) = simplifyArgs sTrm' nextVar
      (cBlocks, _      ) = explicateControl sTrm'' (C.Label startBlockName) 0
      pBlocks            = selectInstructions cBlocks
      (pBlocks', stackSpace) =
          assignHomes (P.Label startBlockName) availableRegs pBlocks
      pBlocks''   = M.map patchInstructions pBlocks'
      stackSpace' = adjustStackSpace stackSpace
      main        = mainBlock stackSpace' (P.Label startBlockName)
      conclusion  = conclusionBlock stackSpace'
  in  showProgram main conclusion pBlocks''

availableRegs :: [P.Reg]
availableRegs =
  [P.RegRDX, P.RegRCX, P.RegRSI, P.RegRDI, P.RegR8, P.RegR9, P.RegR10, P.RegR11]

adjustStackSpace :: Int -> Int
adjustStackSpace stackSpace =
  if stackSpace `mod` 16 == 0 then stackSpace else stackSpace + 8

mainBlock :: Int -> P.Label -> (P.Label, [P.Instr])
mainBlock stackSpace jumpTo = ((P.Label "main"), instrs)
 where
  instrs =
    [ P.InstrPushQ (P.ArgReg P.RegRBP)
    , P.InstrMovQ (P.ArgReg P.RegRSP) (P.ArgReg P.RegRBP)
    , P.InstrSubQ (P.ArgInt stackSpace) (P.ArgReg P.RegRSP)
    , P.InstrJmp jumpTo
    ]

conclusionBlock :: Int -> (P.Label, [P.Instr])
conclusionBlock stackSpace = ((P.Label "conclusion"), instrs)
 where
  instrs =
    [ P.InstrAddQ (P.ArgInt stackSpace) (P.ArgReg P.RegRSP)
    , P.InstrPopQ (P.ArgReg P.RegRBP)
    , P.InstrRetQ
    ]

showBlock :: (P.Label, [P.Instr]) -> String
showBlock (label, instrs) = unlines (fmtLabel : fmtInstrs)
 where
  fmtLabel  = show label ++ ":"
  fmtInstrs = fmap (\instr -> "    " ++ show instr) instrs

showBlocks :: M.Map P.Label [P.Instr] -> String
showBlocks blocks = unlines $ map showBlock $ M.toAscList blocks

showProgram
  :: (P.Label, [P.Instr])
  -> (P.Label, [P.Instr])
  -> M.Map P.Label [P.Instr]
  -> String
showProgram main conclusion prog =
  showBlocks prog
    ++ "    .globl main\n"
    ++ showBlock main
    ++ showBlock conclusion
