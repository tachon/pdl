module Main where

import qualified Data.Set as Set
import qualified Data.List as List
import Debug.Trace

import AST
import ValidityChecking
import Example
import ViewDetermination

main = do
  print $ validityChecking rex cex
{-
  print rex
  print $ reversedRules rex
  print $ normalize $ reversedRules rex
-}
