module Main where

import qualified Data.Set as Set
import qualified Data.List as List
import Data.Text.Internal as Text
import Debug.Trace
import System.IO
import System.Process
import System.Exit
import System.Environment
import System.IO
import System.IO.Error
import Control.Applicative                                   
import Language.Maude.Exec

import AST
import SyntacticConstraints
import Example
import Totality
import ViewDetermination
import SourceStability

csiFile="singleValue.trs"
citpFile="maude27-linux/citp/put.maude"
maudeResFile="maude.txt"

input = ipt0
predicates   = prd  input
functions    = funs input
constructors = ctrs input
rules        = rls  input

main = do

  if syntacticConstraint functions constructors rules then
    putStrLn "Syntactic Constraint..........................ok"
    else do
    putStrLn "Syntactic Constraints are not respected"
    exitFailure

  if totalityChecking predicates functions constructors rules then
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

  if (head $ lines stdo) == "YES" then
    putStrLn "Rput Single-Valued............................ok"
    else
    do
      putStrLn ("The reverse rule is found not confluent by CSI"
                ++ "\nCSI output :\n" ++ stdo)
      exitFailure
      
    
  let (s,n) = writeCITPFile functions constructors rules rrules  

  writeFile citpFile s
    
  (inn, out, err, idd) <- runInteractiveCommand
                          ("./maude27-linux/maude.linux64 "
                           ++ "-no-mixfix -no-ansi-color "
                           ++ "maude27-linux/citp/ui.maude "
                           ++ citpFile)
  
  mapM_ (flip hSetBinaryMode False) [inn, out, err]             
  hSetBuffering inn LineBuffering                          
  hSetBuffering out NoBuffering
  hPutStrLn inn "quit"
  waitForProcess idd
  res <- hGetContents out                      
  writeFile maudeResFile res

  if n == (nbProof res) - 2 then
    putStrLn "Source Stability..............................ok"
    else do
    putStrLn ("Source Stability not true : only " ++ (show $ nbProof res)
              ++ "/" ++ show (n+2) ++ " goals proved.")
    exitFailure

  
  putStrLn "put function is valid"
  
  
{-


  res <- runMaude
             (MaudeConf{maudeCmd="./maude27-linux/maude.linux64",
                        loadFiles=["maude27-linux/citp/ui.maude",citpFile]}) (Rewrite Text.empty)
`catchIOError` (putStrLn $ Text.showText $ maudeFailureStderr)
          
  writeFile maudeResFile $ Text.showText $ maudeStdout res

-}
  
nbProof s =
  List.length $ filter ( \l -> l == "INFO: PROOF COMPLETED!"
                       ) (lines s)
  
