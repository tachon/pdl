module Main where

import qualified Data.Set as Set
import qualified Data.List as List
import Debug.Trace

import AST
import ValidityChecking

import ViewDetermination

main = do
  print $ validityChecking rex
{-
  print rex
  print $ reversedRules rex
  print $ normalize $ reversedRules rex
-}
