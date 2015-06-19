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

csiFile="singleValue.trs"

constructors=cex
rules=rex

main = do

  if syntacticConstraint constructors rules then
    putStrLn "Syntactic Constraint..........................ok"
    else
    putStrLn "Syntactic Constraints not respected"

  if totalityChecking constructors rules then
    putStrLn "Totality Checking.............................ok"
    else
    putStrLn "This put function is not total"

  if putSInjective rules then
    putStrLn "(Put s) Injective.............................ok"
    else
    putStrLn "(Put s) is not injective"

  writeFile csiFile $ rulesToCSIFile
    $ normalize $ reversedRules rex
  (exitCode,stdo,stdr) <- readProcessWithExitCode
                          "csi/csi.sh" [csiFile] ""
  case exitCode of
    ExitSuccess ->
      putStrLn "Rput Single-Valued............................ok"
    _ -> print ("The reverse rule is found not confluent by CSI"
                ++ "\nCSI output :\n" ++ stdo
                ++ "\nCSI errors :\n" ++ stdr)
