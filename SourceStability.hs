module SourceStability where

import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map

import AST
import Example
import PatExhaustiveness

writeBeginCITPFile constructors rules rrules  =
  "load ui\n\n(fmod PUT is\n" --PUT to be changed
  ++ "sorts " ++ allTypes constructors ++ " .\n---\n"
  ++ writeConst constructors ++ "---\n"
  ++ writeVars (getAllVars constructors rules rrules) ++ "---\n"
  ++ writeRules rules ++ "\n"
  ++ writeRRules rrules ++ "\n"
  ++ writeSSProperty (name $ head rules) (rn $ head rrules)
  ++ "\n" 


{-writeCITPFile constructors rules rrules =
  writeBeginCITPFile constructors rules rrules
  ++ addedLemma ++ "\nendfm)\n" ++ goal
-}

allTypes constructors =
  unwords $ Set.toList $ foldl
  (\acc c -> Set.insert (typ c) acc
  ) Set.empty constructors

writeConst constructors= 
  foldl (\s c ->
          s ++ " op " ++ idt c ++ " : " ++ (unwords $ sub c)
          ++ " -> " ++ typ c ++ " [ctor] .\n"
        ) "" constructors

getAllVars cons rules rrules =
  foldl (
    \map r -> 
    let (_,map1) = patWellTyped cons typeofPS (ps r) Map.empty
        (_,map2) = patWellTyped cons typeofPV (pv r) map1
        --hoping same name -> same type : to be checked 
    in Map.union map map2
    ) Map.empty rules
  `Map.union`
  foldl (
    \map r -> 
    let (_,map1) = patWellTyped cons typeofExpr (ip r) map 
    in Map.union map map1
    ) Map.empty rrules 
          

writeVars vars =
  Map.foldWithKey
  (\t lv s ->
    " vars " ++ unwords lv ++ " : " ++ t ++ " .\n" ++ s
  ) "" (Map.foldWithKey
  (\v t m -> Map.insertWith (++) t [v] m) Map.empty vars)

writeRules rules =
  "op " ++ (name $ head rules) ++ " : " ++ typeofPS ++ " " ++
  typeofPV ++ " -> " ++ typeofExpr ++ " .\n" ++
  (unlines $ map (\r -> " eq " ++ show r ++ " .") rules)

writeRRules rrules =
  " op " ++ (rn $ head rrules) ++ " : " ++ 
  typeofExpr ++ " -> " ++ typeofPV ++ " .\n" ++
  (unlines $ map (\r -> " eq " ++ show r ++ " .") rrules)

writeSSProperty rulesName rrulesName=
  " op pr : " ++
  typeofExpr ++ " " ++
  typeofExpr ++ " -> " ++
  typeofExpr ++ " .\n" ++
  " eq pr(x,y) = " ++ rulesName ++
  "(x," ++ rrulesName ++ "(y)) .\n"
