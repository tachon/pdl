module SyntacticConstraints where

import AST
import Totality

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List


syntacticConstraint funs cons rules =
 argFunInPat rules
 && isAffine rules
 && goodNbSubInRules cons rules
 && goodName funs rules

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


goodName funs rules =
  myError "A Function is not defined" $ 
  and $ map (\r -> any ((==) (name r) . fName) funs) rules


toStringp (Cons id vp) =
  if length vp == 0 then id else
    id ++ "(" ++ (List.intercalate "," $ map toStringp vp) ++ ")"
toStringp (LAV _ p)    = toStringp p
toStringp (Var s  )    = s


toStringre (CRE id ve) =
  if length ve == 0 then id else
    id ++ "(" ++ (List.intercalate "," $ map toStringre ve) ++ ")"
toStringre (VarRE s )  = s
toStringre (FunRE name arg) =
  name ++ "(" ++ arg ++ ")"

toStringe env (CE id ve) =
  if length ve == 0 then id else
    id ++ "(" ++ (List.intercalate "," $ map (toStringe env) ve) ++ ")"
toStringe env (VarE s) =
  if Map.member s env then toStringp (env Map.! s) else s  
toStringe env (Fun name arg1 arg2) =
  name ++ "(" ++
  (if Map.member arg1 env then
     toStringp (env Map.! arg1)
   else arg1)
  ++ "," ++
  (if Map.member arg2 env then
     toStringp (env Map.! arg2)
   else arg2)
  ++ ")"

getLAV rule =
  (getLAVP $ ps rule)
  `Map.union`
  (getLAVP $ pv rule)
  
  
getLAVP (Cons _ lp) = Map.unions $ map getLAVP lp
getLAVP (LAV s p)   = Map.singleton s p 
getLAVP (Var _)     = Map.empty
