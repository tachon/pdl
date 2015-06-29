module ValidityChecking where

import AST
import PatExhaustiveness

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.List as List


syntacticConstraint cons rules =
 argFunInPat rules
 && isAffine rules
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
        (Set.member a1 (getVariables $ ps r) &&
         Set.member a2 (getVariables $ pv r))
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

isAffine rules =
  and $ map
  (\r -> let e = xpr r in
    myError ("In right side of rule :\n" ++ show r)
    (fst $ affineAux (getVariables e) e)) rules

affineAux set (Fun _ s1 s2) =
  ((myError ("The variable " ++ show s1 ++
             " must appear only once")
   (Set.member s1 set)) &&
  (myError ("The variable " ++ show s2 ++
            " must appear only once")
   (Set.member s2 set)), (Set.delete s2 $ Set.delete s1 set))

affineAux set (VarE s)      =
  (myError ("The variable " ++ show s ++ " must appear only once")
  $ Set.member s set , Set.delete s set)

affineAux set (CE _ xp)     =
  foldl (\(b1, set1) p ->
          let (b2,set2) = affineAux set1 p in
          (b1 && b2, set2) 
        ) (True, set) xp


goodNbSubInRules cons rules =
  and $ map (\r -> myError ("In rule\n" ++ show r)
                   ((   goodNumberSub cons $ ps r)
                    && (goodNumberSub cons $ pv r)
                    && (goodNumberSub cons $ xpr r))) rules


toStringp (Cons id vp) =
  id ++ "(" ++ (List.intercalate "," $ map toStringp vp) ++ ")"
toStringp (LAV _ p)    = toStringp p
toStringp (Var s  )    = s


toStringre (CRE id ve) =
  id ++ "(" ++ (List.intercalate "," $ map toStringre ve) ++ ")"
toStringre (VarRE s )  = s
toStringre (FunRE name arg) =
  name ++ "(" ++ arg ++ ")"

toStringe (CE id ve) =
  id ++ "(" ++ (List.intercalate "," $ map toStringe ve) ++ ")"
toStringe (VarE s ) = s
toStringe (Fun name arg1 arg2) =
  name ++ "(" ++ arg1 ++ "," ++ arg2 ++ ")"
