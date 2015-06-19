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

main = do
  --print $ validityChecking rex cex


  --print "Rules :"
  --print rex
  --print "Reversed rules :"
  --print $ reversedRules rex
  --print "Normalized rules :"
  writeFile csiFile $ rulesToCSIFile
    $ normalize $ reversedRules rex
  (exitCode,stdo,stdr) <- readProcessWithExitCode "csi/csi.sh" [csiFile] ""
  case exitCode of
    ExitSuccess -> putStrLn "View determination ok !"
    _ -> print ("The reverse rule is judge not confluent by CSI"
           ++ "\nCSI output :\n" ++ stdo
           ++ "\nCSI errors :\n" ++ stdr)
