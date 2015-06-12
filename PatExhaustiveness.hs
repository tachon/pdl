module PatExhaustiveness where

import qualified Data.Set as Set
import qualified Data.List as List
import Debug.Trace

import AST

data P = Cst ID [P] | Wildcard deriving(Eq)

instance Show P where
  show Wildcard = "_"
  show( Cst i pl) = "C " ++ show i ++ " " ++ show pl

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

createSigma :: [[P]] -> Set.Set C
createSigma [] = Set.empty
createSigma ([]:_) = mytrace "Error : one line is empty" Set.empty
createSigma ((p:pline):pcol) =
  case p of
    Cst id _ -> Set.insert (getC id) (createSigma pcol)
    Wildcard  -> Set.empty

isCompleteSig :: Set.Set C -> Bool
isCompleteSig sigma =
  (not $ Set.null sigma) && 
  (and $ map (\c -> Set.member c sigma)
  (consOfType $ typ $ head $ (Set.toList sigma)))


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
  

u m []               = mytrace ("end M = " ++ (show m))
                       (length m == 0)
u m ((Cst id pv):v) =
  let c = getC id
      _ = verifyInvariant m ((Cst id pv):v) 0
      
  in u (simplifyM c m) (simplifyV c ((Cst id pv):v))

u m (Wildcard:v)     =
  let sigma = createSigma m
      _     = mytrace ("sigma = " ++ Set.showTree sigma)
              verifyInvariant m (Wildcard:v) 0 in
  if (isCompleteSig sigma) then
    or $ map (\c -> u (simplifyM c m) (simplifyV c (Wildcard:v)) 
             ) (Set.toList sigma)
  else
    u (defaultM m) v


allRulesUsefull rules =
  let m = ruleToMat rules in
  foldr (\vp acc ->
          acc && 
          (myError ("This pattern :\n" ++ showPsPv vp ++
                    "\nWill be ignored because it is " ++
                    "overlapping with ohers")                
           (u (List.delete vp m) vp))
        ) True m 
    
notInSigma sigma =
  let allsig = (consOfType $ typ $ head $ (Set.toList sigma))
  in head $ Set.fold List.delete allsig sigma


i m 0 = if length m == 0 then Just [] else Nothing
i m n =
  let sigma = createSigma m in
  if (isCompleteSig sigma) then
    mytrace ("Complete sig, M=\n" ++ (show m)
            ++ "\nn = " ++ (show n))
    (findNEC m n (Set.toList sigma))
  else
    case i (defaultM m) (n-1) of
      Nothing -> Nothing
      Just vp ->
        if Set.null sigma then
          Just $ Wildcard:vp
        else
          let c = notInSigma sigma in
          Just $ (Cst (idt c) (take (arity c)
                                (repeat Wildcard))) : vp
    

findNEC m n []    = Nothing
findNEC m n (c:v) =
  let m1 = mytrace2 "findNEC : simplifiedM : \n" (simplifyM c m)
      c1 = n - 1 + arity (mytrace2 "with c = " c) 
  in case i m1 c1 of
    Nothing -> findNEC m n v
    Just vp ->
      let (h,t) = splitAt (arity c) vp in
      Just $ (Cst (idt c) h) : t


wellTyped rules =
  and $ map (
    \r ->
    (myError ("in source Pattern of rule :\n" ++ show r)
     (patWellTyped typeofPS (ps r))) &&
    
    (myError ("in view Pattern of rule :\n" ++ show r)
     (patWellTyped typeofPV (pv r))) &&

    (myError ("in right hand side of rule :\n" ++ show r)
     (exprWellTyped typeofExpr (xpr r)))
    ) rules 


exprWellTyped t (Fun n v1 v2) =
  (myError ("This function call " ++ show (Fun n v1 v2)
            ++ " \tis of type " ++ show typeofExpr
            ++ " but is supposed to be of type " ++ show t)
   (t == typeofExpr))
exprWellTyped _ (VarE _)    = True
exprWellTyped t (CE i vp)   =
  let c = getC i
  in (myError ("This expression : " ++ show (CE i vp)
               ++ " \tis of type " ++ show (typ c)
               ++ " but is supposed to be of type " ++ show t)
      (t == typ c)) &&
     (and $ map(\(t1, p1) ->
                 exprWellTyped t1 p1) (zip (sub c) vp))


patWellTyped _ (Var _)     = True
patWellTyped t (LAV _ p)   = patWellTyped t p
patWellTyped t (Cons i vp) =
  let c = getC i
  in (myError ("This pattern : " ++ show (Cons i vp)
               ++ " \tis of type " ++ show (typ c)
               ++ " but is supposed to be of type " ++ show t)
      (t == typ c))
     && (and $ map(\(t1, p1) ->
                    patWellTyped t1 p1) (zip (sub c) vp))


showPsPv [a,b] = "Fun \t(" ++ show a ++ ") \t(" ++ show b ++ ")"
showPsPv l     = show l
