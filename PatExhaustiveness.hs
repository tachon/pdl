module PatExhaustiveness where

import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import Debug.Trace

import AST
import Example

data P = Cst ID [P] | Wildcard deriving(Eq)

instance Show P where
  show Wildcard = "_"
  show( Cst i pl) = "C " ++ show i ++ " " ++ show pl

totalityChecking funs cons rules =
  wellTyped funs cons rules &&
  --allRulesUsefull cons rules &&
  case i cons (ruleToMat rules) 2 of
    Nothing -> True
    Just p  -> trace ("Pattern not exhaustive \n"
                      ++ "This pattern is not represented :\n"
                      ++ (showPsPv p))
               False

verifyInvariant [] [] value =
  mytrace "m and v empty" value
verifyInvariant m v value =
  if and $ map (\row -> length row == length v) m
  then mytrace ("M = " ++ show m ++ "V = " ++ show v) value
  else mytrace ("M = " ++ show m ++ "V = " ++ show v) $
       mytrace "1 row of m has not the same length as v"
       value

prToP (Cons i lps)= Cst i (map prToP lps)
prToP (LAV _ pfs) = prToP pfs 
prToP (Var _)     = Wildcard

ruleToMat []     = [] 
ruleToMat (r:rs) =
  mytrace (" row = " ++ (show [(prToP $ ps r) , (prToP $ pv r)]))
  [(prToP $ ps r) , (prToP $ pv r)]
  : ruleToMat rs

createSigma _ [] = Set.empty
createSigma _ ([]:_) = mytrace "Error : one line is empty"
                       Set.empty
createSigma cons ((p:pline):pcol) =
  case p of
    Cst id _ -> Set.insert (getC cons id) (createSigma cons pcol)
    Wildcard  -> Set.empty

isCompleteSig cons sigma =
  (not $ Set.null sigma) && 
  (and $ map (\c -> Set.member c sigma)
  (consOfType cons $ typ $ head $ (Set.toList sigma)))


simplifyVM c []                =
  mytrace ("v empty when testing "++ show c) Just []
  
simplifyVM c (Wildcard:pv1)    =
  Just $ take (arity c) (repeat Wildcard) ++ pv1

simplifyVM c ((Cst id pv2):pv1) =
  if (idt c) == id then
    Just (pv2 ++ pv1)
  else Nothing

simplifyV c []                 =
  mytrace ("v empty when testing "++ show c) []
  
simplifyV c (Wildcard:pv1)     =
 take (arity c) (repeat Wildcard) ++ pv1

simplifyV _ ((Cst _ pv2):pv1) = pv2 ++ pv1


simplifyM c []    = []
simplifyM c (v:m) =
  case (simplifyVM c v) of
    Nothing -> simplifyM c m
    Just v1 -> v1 : simplifyM c m
  
defaultM []                = []
defaultM (((Cst _ _):v):m) = defaultM m
defaultM ((Wildcard:v):m)  = v : defaultM m
  

u _ m []               = mytrace ("end M = " ++ (show m))
                       (length m == 0)
u cons m ((Cst id pv):v) =
  let c = getC cons id
      _ = verifyInvariant m ((Cst id pv):v) 0
      
  in u cons (simplifyM c m) (simplifyV c ((Cst id pv):v))

u cons m (Wildcard:v)     =
  let sigma = createSigma cons m
      _     = mytrace ("sigma = " ++ Set.showTree sigma)
              verifyInvariant m (Wildcard:v) 0 in
  if (isCompleteSig cons sigma) then
    or $ map (\c -> u cons (simplifyM c m)
                    (simplifyV c (Wildcard:v)) 
             ) (Set.toList sigma)
  else
    u cons (defaultM m) v


allRulesUsefull cons rules =
  let m = ruleToMat rules in
  foldr (\vp acc ->
          acc && 
          (myError ("This pattern :\n" ++ showPsPv vp ++
                    "\nWill be ignored because it is " ++
                    "overlapping with ohers")                
           (u cons (List.delete vp m) vp))
        ) True m 
    
notInSigma cons sigma =
  let allsig = (consOfType cons $ typ $ head $ (Set.toList sigma))
  in head $ Set.fold List.delete allsig sigma


i _ m 0    = if length m == 0 then Just [] else Nothing
i cons m n =
  let sigma = createSigma cons m in
  if (isCompleteSig cons sigma) then
    mytrace ("Complete sig, M=\n" ++ (show m)
            ++ "\nn = " ++ (show n))
    (findNEC cons m n (Set.toList sigma))
  else
    case i cons (defaultM m) (n-1) of
      Nothing -> Nothing
      Just vp ->
        if Set.null sigma then
          Just $ Wildcard:vp
        else
          let c = notInSigma cons sigma in
          Just $ (Cst (idt c) (take (arity c)
                                (repeat Wildcard))) : vp
    

findNEC _ _ _ []       = Nothing
findNEC cons m n (c:v) =
  let m1 = mytrace2 "findNEC : simplifiedM : \n" (simplifyM c m)
      c1 = n - 1 + arity (mytrace2 "with c = " c) 
  in case i cons m1 c1 of
    Nothing -> findNEC cons m n v
    Just vp ->
      let (h,t) = splitAt (arity c) vp in
      Just $ (Cst (idt c) h) : t


wellTyped funs cons rules =
  Map.foldWithKey
  (\f rofs b ->
    and $ map (
      \r ->
      let (b1, env1) =
            (patWellTyped cons (tps f) (ps r) Map.empty)
          (b2, env2) =
            (patWellTyped cons (tpv f) (pv r) Map.empty)
      in (myError ("in source Pattern of rule :\n" ++ show r) b1)
         && (myError ("in view Pattern of rule :\n" ++ show r) b2)
         && (myError ("in right hand side of rule :\n" ++ show r)
             (exprWellTyped cons f (txp f) (xpr r)
              (Map.union env1 env2)))
      ) rules 
  ) True (rulesOfFuns funs rules) 

patWellTyped _    t (Var s)     env = (True, Map.insert s t env)
patWellTyped cons t (LAV s p)   env = (True, Map.insert s t env)
patWellTyped cons t (Cons i vp) env =
  let c = getC cons i
      b = (myError ("This pattern : " ++ show (Cons i vp)
               ++ " \tis of type " ++ show (typ c)
               ++ " but is supposed to be of type " ++ show t)
           (t == typ c))
      (b1, env1) = foldl
                   (\(wt, map) (t1, p1) ->
                     let (wt1, map1) = patWellTyped cons t1 p1 env
                     in (wt && wt1, Map.union map map1 )
                   ) (True, env) (zip (sub c) vp)
  in (b && b1, env1)
--     (and $ map(\(t1, p1) ->
--                    patWellTyped cons t1 p1) (zip (sub c) vp))


exprWellTyped _ f t (Fun n v1 v2) env =
  (myError ("This function call " ++ show (Fun n v1 v2)
            ++ " \tis of type " ++ show t
            ++ " but is supposed to be of type "
            ++ show (txp f))
   (t == (txp f)))
  && Map.member v1 env &&
  let t1 = env Map.! v1 in
  (myError ("The variable " ++ show v1
            ++ " in function call " ++ show (Fun n v1 v2)
            ++ " \tis of type " ++ show t1
            ++ " but is supposed to be of type "
            ++ show (tps f))
   (t1 == (tps f)))
  && Map.member v2 env &&
  let t1 = env Map.! v2 in
  (myError ("The variable " ++ show v2
            ++ " in function call " ++ show (Fun n v1 v2)
            ++ " \tis of type " ++ show t1
            ++ " but is supposed to be of type "
            ++ show (tpv f))
   (t1 == (tpv f)))
  
exprWellTyped _ _ t (VarE v) env =
  let t1 = env Map.! v in
  (myError ("The variable " ++ show v
            ++ " does not exist in left side"
            ++ " (or is looked-ahead by another variable)")
   (Map.member v env)) &&
  (myError ("The variable " ++ show v
            ++ " \tis of type " ++ show t1
            ++ " but is supposed to be of type " ++ show t)
   (t1 == t))

exprWellTyped cons f t (CE i vp) env =
  let c = getC cons i
  in (myError ("This expression : " ++ show (CE i vp)
               ++ " \tis of type " ++ show (typ c)
               ++ " but is supposed to be of type " ++ show t)
      (t == typ c)) &&
     (and $ map(\(t1, p1) ->
                 exprWellTyped cons f t1 p1 env) (zip (sub c) vp))



showPsPv [a,b] = "F   (" ++ show a ++ ")   ("
                 ++ show b ++ ")  =  e"
showPsPv l     = show l

rulesOfFuns funs rules =
  foldl (\map f ->
          Map.insert f
          (filter ((fName f ==) . name) rules)
          map) Map.empty funs
