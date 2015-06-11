module ValidityChecking where

import AST
import PatExhaustiveness
import ViewDetermination

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.List as List


syntacticConstraint rules =
 argFunInPat rules

totalityChecking rules =
  wellTyped rules &&
  case i (ruleToMat rules) 2 of
    Nothing -> True
    Just p  -> trace ("Pattern not exhaustive \n"
                      ++ "This pattern is not represented :\n "
                      ++ (show p))
               False


  
validityChecking rules =
  syntacticConstraint rules &&
--Check Totality
  totalityChecking rules &&
--Check View Determination
  putSInjective rules



argFunInPat rules =
  and $ map
  (\r ->
    case getArgFun $ xpr r of
      Nothing -> True
      Just (a1, a2) ->(
        myError ("Arguments " ++ show a1
                 ++ " and " ++ show a2
                 ++ " in right-hand side of rule :\n"
                 ++ show r
                 ++ "Must be variables which exists" 
                 ++ " in their respective patterns")
        (Set.member a1 (getVariablesP $ ps r) &&
         Set.member a2 (getVariablesP $ pv r))
        ) &&
        myError ("As there is a function call in rule :\n"
                 ++ show r
                 ++ "At least one of its argumentxs must be"
                 ++ " strictly smaller that its original pattern"
                 ++ "\nIf not, function would loop infinitly")
        (case (ps r, pv r) of
            (Var a1 , Var a2 )   -> False
            (LAV a1 _, Var a2)   -> False
            (Var a1, LAV a2 _)   -> False
            (LAV a1 _, LAV a2 _) -> False
            _                  -> True)
        
  ) rules


