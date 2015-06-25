module ViewDetermination where

import qualified Data.Set as Set
import qualified Data.List as List
import Debug.Trace

import AST

newVar = "ss'"

pvInXP rule =
  myError
  ("All variables in view pattern must appear in " ++
   "right hand side expression to satisfy " ++
   "view determination in rule :\n" ++ show rule) 
  ((getVariables $ pv $ rule)
  `Set.isSubsetOf`  
  (getVariables $ xpr $ rule))
    
putSInjective rules = and $ map pvInXP rules

exprToPat (CE i xpl)  = Cons i $ map exprToPat xpl 
exprToPat (VarE s)    = Var s
exprToPat (Fun _ _ _) = Var newVar

patToRExpr s f (Cons i vp)        = CRE i $ map (patToRExpr s f) vp
patToRExpr (Just s) f (LAV s1 _ ) =
  if s1 == s then FunRE f newVar
  else VarRE s1
patToRExpr (Just s) f (Var s1   ) =
  if s1 == s then FunRE f newVar
  else VarRE s1

nameOfPV (VarE s)    = Nothing
nameOfPV (Fun _ _ s) = Just s
nameOfPV (CE i xpl)  =
  foldl (\s1 xp ->
          case nameOfPV xp of
            Nothing -> s1
            Just vs -> Just vs
        ) Nothing xpl 
  

reversedRules rules =
  map (\r ->
        let vs = nameOfPV $ xpr $ r 
            f = 'R': (name r)
        in
         RRule {rn = f,
                ip   = exprToPat $ xpr r,
                op   = patToRExpr vs f (pv r)
               }
      ) rules


doNothing r =
  case op r of
    FunRE _ s2 -> 
      case ip r of
        Var s1 -> s1 == s2
        _      -> False
    _       -> False
normalize rrules =
  filter (not . doNothing) (List.nub rrules)
  

getAllRRulesVar rrules =
  foldl (\vars rr ->
          (getVariables $ ip rr)
          `Set.union` (getVariables $ op rr)
          `Set.union` vars) Set.empty rrules


rulesToCSIFile rrules =
  let vs = List.intercalate " " $
           Set.toList $
           getAllRRulesVar rrules in
  "(VAR " ++ vs ++ ")"
  ++ "\n(RULES\n" ++
  (foldl (\str rr -> str ++
           "  " ++ (rn rr) ++ "("
           ++ (toCSIp $ ip rr)
           ++ ") -> "
           ++ (toCSIre $ op rr)
           ++ "\n"
         ) "" rrules)
  ++ ")"

toCSIp (Cons id vp) =
  id ++ "(" ++ (List.intercalate "," $ map toCSIp vp) ++ ")"
toCSIp (LAV _ p)    = toCSIp p
toCSIp (Var s  )    = s


toCSIre (CRE id ve)      =
  id ++ "(" ++ (List.intercalate "," $ map toCSIre ve) ++ ")"
toCSIre (VarRE s  )      = s
toCSIre (FunRE name arg) =
  name ++ "(" ++ arg ++ ")"
