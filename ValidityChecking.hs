module ValidityChecking where

import AST
import PatExhaustiveness
import ViewDetermination

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.List as List


syntacticConstraint cons rules =
 argFunInPat rules
 && goodNbSubInRules cons rules

totalityChecking cons rules =
  wellTyped cons rules &&
  allRulesUsefull cons rules &&
  case i cons (ruleToMat rules) 2 of
    Nothing -> True
    Just p  -> trace ("Pattern not exhaustive \n"
                      ++ "This pattern is not represented :\n"
                      ++ (showPsPv p))
               False


  
validityChecking rules constructors =
  syntacticConstraint constructors rules &&
--Check Totality
  totalityChecking constructors rules &&
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
                 ++ "At least one of its arguments must be"
                 ++ " strictly smaller that its original pattern"
                 ++ "\nIf not, function would loop infinitly")
        (case (ps r, pv r) of
            (Var a1 , Var a2 )   -> False
            (LAV a1 _, Var a2)   -> False
            (Var a1, LAV a2 _)   -> False
            (LAV a1 _, LAV a2 _) -> False
            _                    -> True)        
  ) rules



goodNbSubInRules cons rules =
  and $ map (\r -> myError ("In rule\n" ++ show r)
                   ((   goodNumberSub cons $ ps r)
                    && (goodNumberSub cons $ pv r)
                    && (goodNumberSub cons $ xpr r))) rules

 
instance PatExpr Pat where
  goodNumberSub _ (Var _)        = True
  goodNumberSub cons (LAV _ p)   = goodNumberSub cons p
  goodNumberSub cons (Cons i vp) =
    let c    = getC cons i
        lsub = length $ sub c
        lvp  = length vp
    in (myError ("Constructor " ++ show i ++
                " should be applicate to " ++ show lsub ++
                " patterns but here is applicate to " ++ 
                show lvp ++ " patterns")
        (lvp == lsub)
       ) && (and $ map (goodNumberSub cons) vp)

instance PatExpr Expr where
  goodNumberSub _ (VarE _)     = True
  goodNumberSub _ (Fun _ _ _)  = True
  goodNumberSub cons (CE i vp) =    
    let c    = getC cons i
        lsub = length $ sub c
        lvp  = length vp
    in (myError ("Constructor " ++ show i ++
                 " should be applicate to " ++ show lsub ++
                 " patterns but here is applicate to " ++ 
                 show lvp ++ " patterns")
        (lvp == lsub)
       ) && (and $ map (goodNumberSub cons) vp)
