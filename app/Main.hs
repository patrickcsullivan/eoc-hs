module Main where

import           Driver

import qualified CIR.AST                       as C
import           CIR.InstrSelector              ( selectInstructions )
import           CIR.VarUncoverer               ( uncoverVars )
import qualified RIR.AST                       as R
import           RIR.ArgUniquifier              ( uniquifyArgs )
import           RIR.ArgSimplifier              ( simplifyArgs )
import           RIR.ControlExplicator          ( explicateControl )
import qualified PXIR.AST                      as P
import           PXIR.HomeAssigner              ( assignHomesInBlock )
import           PXIR.InstrPatcher              ( patchInstructions )

main :: IO ()
main = do
  rslt <- driveIO trm
  putStrLn rslt

trm = R.TermLet
  (R.Var "my_var")
  (R.TermVal (R.ValueInt 42))
  (R.TermLet
    (R.Var "input")
    R.TermRead
    (R.TermLet
      (R.Var "my_var")
      (R.TermAdd (R.TermVar (R.Var "my_value"))
                 (R.TermNeg (R.TermVar (R.Var "input")))
      )
      (R.TermVar (R.Var "my_value"))
    )
  )

driveIO :: R.Term -> IO String
driveIO rTrm = do
  let (rTrm', nextVar) = uniquifyArgs rTrm 0
  let (rTrm'', _)      = simplifyArgs rTrm nextVar
  putStrLn $ show rTrm''
  let cTail     = explicateControl rTrm''
  let localVars = uncoverVars cTail
  putStrLn $ show cTail
  let pInstrs = selectInstructions cTail
  let pBlock  = P.Block (P.Label "start") pInstrs
  putStrLn $ show pBlock
  let (pBlock', stackSpace) = assignHomesInBlock pBlock
  putStrLn $ show pBlock'
  let stackSpace' = adjustStackSpace stackSpace
  let pBlock''    = patchInstructions pBlock'
  let main        = mainBlock stackSpace' (P.Label "start")
  let conclusion  = conclusionBlock stackSpace'
  return $ writeBlocks main conclusion pBlock''
