module Main where

import qualified Data.Set as Set
import qualified Data.List as List
import Debug.Trace
import System.IO
import System.Process
import System.Exit

import AST
import ValidityChecking
import Example
import ViewDetermination
import SourceStability

csiFile="singleValue.trs"
citpFile="put.maude"

constructors=cex
rules=rex

main = do

  if syntacticConstraint constructors rules then
    putStrLn "Syntactic Constraint..........................ok"
    else do
    putStrLn "Syntactic Constraints are not respected"
    exitFailure

  if totalityChecking constructors rules then
    putStrLn "Totality Checking.............................ok"
    else do
    putStrLn "This put function is not total"
    exitFailure

  if putSInjective rules then
    putStrLn "(Put s) Injective.............................ok"
    else do
    putStrLn "(Put s) is not injective"
    exitFailure

  let rrules = normalize $ reversedRules rules
  writeFile csiFile $ rulesToCSIFile rrules

  (exitCode,stdo,stdr) <- readProcessWithExitCode
                            "csi/csi.sh" [csiFile] ""
  case exitCode of
    ExitSuccess ->
      putStrLn "Rput Single-Valued............................ok"
    _ -> do
      putStrLn ("The reverse rule is found not confluent by CSI"
                ++ "\nCSI output :\n" ++ stdo
                ++ "\nCSI errors :\n" ++ stdr)
      exitFailure
  
  writeFile citpFile $ writeCITPFiles constructors rules rrules  
      
