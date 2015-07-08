module SourceStability where

import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import Debug.Trace
import Data.Char

import AST
import Example
import PatExhaustiveness
import ValidityChecking

data Tactics = SI | CA | TC | IP | CS | RD | AUTO 

type Ind = Bool

data GoalPat = ConsG ID [GoalPat]
             | VarG String Tip Ind


data Goal = EQU  Int String String GoalPat
          | CEQ Int String String GoalPat GoalPat
          | PR  Int Tip
            
debug = True

writeCITPFile csts rules rrules =
  let (s,n) = writeGoals csts rrules (name $ head rules)
              (rn $ head rrules)
  in (writePUTmod csts rules rrules ++ s, n)
  
writePUTmod constructors rules rrules  =
  "(fmod PUT-0 is\n" --PUT to be changed
  ++ " sorts " ++ allTypes constructors ++ " .\n---\n"
  ++ writeConst constructors ++ "---\n"
  ++ writeVars (getAllVars constructors rules rrules) ++ "---\n"
  ++ writeRules rules ++ "\n"
  ++ writeRRules rrules ++ "\n"
  ++ writeSSProperty (name $ head rules) (rn $ head rrules)
  ++ "\nendfm)\n\n" 

allTypes constructors =
  unwords $ Set.toList $ foldl
  (\acc c -> Set.insert (typ c) acc
             `Set.union` (Set.fromList $ sub c)
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
  `Map.union`
  (Map.fromList [("x",typeofExpr), ("y",typeofExpr)])

writeVars vars =
  Map.foldWithKey
  (\t lv s ->
    " vars " ++ unwords lv ++ " : " ++ t ++ " .\n" ++ s
  ) "" (Map.foldWithKey
  (\v t m -> Map.insertWith (++) t [v] m) Map.empty vars)

writeRules rules =
  " op " ++ (name $ head rules) ++ " : " ++ typeofPS ++ " " ++
  typeofPV ++ " -> " ++ typeofExpr ++ " .\n" ++
  (unlines $ map
   (\r ->
     let env = getLAV r in
     " eq " ++ (name r) ++ "(" 
          ++ (toStringp $ ps r) ++ ","
          ++ (toStringp $ pv r) ++ ") = "
          ++ (toStringe env (xpr r)) ++ " .")
   rules)

writeRRules rrules =
  " op " ++ (rn $ head rrules) ++ " : " ++ 
  typeofExpr ++ " -> " ++ typeofPV ++ " .\n" ++
  (unlines $ map
   (\rr -> " eq " ++ (rn rr) ++ "(" 
          ++ (toStringp $ ip rr) ++ ") = "
          ++ (toStringre $ op rr) ++ " .")
   rrules)

writeSSProperty rulesName rrulesName=
  " op pr : " ++
  typeofExpr ++ " " ++
  typeofExpr ++ " -> " ++
  typeofExpr ++ " .\n" ++
  " eq pr(x,y) = " ++ rulesName ++
  "(x," ++ rrulesName ++ "(y)) .\n"


writeGoals csts rrules nameR nameRR =
  let lg = intermGoals csts rrules in
  if null lg then ("",0) else
    let (s1, n1) =
          foldl (
            \(s, n) gp ->
            let Just (gp1, gpIH) = findVarIH gp rrules csts
                g = CEQ n nameR nameRR gp1 gpIH
            in (s ++ show g 
            ++ writeInd gp1
            ++ applyTactics [SI,TC,IP,TC,IP]
            ++ createModule g (n+1) nameR nameRR , n+1)
            ) ("", 0) lg 
    in (s1 ++ (lastGoals n1 nameR nameRR), n1)

lastGoals n nameR nameRR =
  let gp = VarG "S" typeofExpr True
      g  = PR (n+1) typeofExpr
  in
  show (EQU n nameR nameRR gp) ++
  writeInd gp ++
  applyTactics [SI, IP] ++
  createModule g (n+1) nameR nameRR ++
  show g ++ applyTactics [TC, IP]
  

writeInd g =
  (Set.foldl (\s (v,t) -> s ++ " " ++ v ++ ":" ++ t) 
  "(set ind on" (findIndVars g ))
  ++ " .)\n"

createModule g n nameR nameRR =
  "(fmod PUT-" ++ show n ++
  " is\n" ++ writeInclude (n-1) ++
  Map.foldWithKey
  (\t vs s ->
    s ++ " vars "
    ++ (List.intercalate " " (Set.toList vs))
    ++ " : " ++ t ++ " .\n"
  ) "" (getGoalVars g)
  ++ "---\n" ++
  showLemma g
  
  
getGoalPVars (ConsG id lg) =
  Map.unionsWith Set.union $ map getGoalPVars lg
getGoalPVars (VarG v t _)  =
  Map.insertWith Set.union t (Set.singleton v) Map.empty

getGoalVars :: Goal -> Map.Map Tip (Set.Set String)
getGoalVars (EQU _ _ _ gp)      = getGoalPVars gp
getGoalVars (CEQ _ _ _ gp1 gp2) =
  Map.unionWith Set.union (getGoalPVars gp1) (getGoalPVars gp1)
getGoalVars (PR _ t)            =
  Map.singleton t (Set.singleton "S")

writeInclude n =
  if n <= 0 then " inc PUT-0 .\n----\n" else
    " inc PUT-" ++ show n ++ " .\n" ++
    writeInclude (n-1)

findVarIH g rrules cons =
  case List.find ((matchGP g) . ip) rrules of 
    Nothing -> Nothing
    Just rr ->
      let (_, env) = patWellTyped cons typeofExpr (ip rr) Map.empty
      in case findRecVar (op rr) of
        Nothing -> Nothing
        Just v  -> Just (putVarInGP g (ip rr) v,
                         VarG v (env Map.! v) False)


putVarInGP (VarG s1 t b)   (Var s2)    v =
  if s2 == v then (VarG v t b) else (VarG s1 t b)
putVarInGP (VarG s t b)   (Cons _ _) _   = (VarG s t b) 
putVarInGP (ConsG i1 lgp) (Var _)    _   = (ConsG i1 lgp) 
putVarInGP (ConsG i1 lgp) (Cons i2 lp) v =
  ConsG i1 (zipWith3 putVarInGP lgp lp
            (take (List.length lgp) (repeat v)))
  
      
findRecVar (VarRE _)   = Nothing
findRecVar (FunRE _ v) = Just v 
findRecVar (CRE _ res) =
  foldl (\acc p -> case findRecVar p of
            Nothing -> acc
            Just v -> Just v
        ) Nothing res

matchGP :: GoalPat -> Pat -> Bool
matchGP g              (LAV _ p)    = matchGP g p
matchGP (VarG _ _ _)   (Var _)      = True
matchGP (VarG _ _ _)   (Cons _ _)   = True
matchGP (ConsG i1 lgp) (Cons i2 lp) =
  i1 == i2 && (and $ zipWith matchGP lgp lp) 
matchGP _ _ = False
    
findIndVars (ConsG _ lg) = Set.unions $ map findIndVars lg 
findIndVars (VarG s t b) = if b then Set.singleton (s,t)
                           else Set.empty
  
applyTactics lt =
  foldl(\s tct -> s ++ show tct ++ " ") "(apply " lt
  ++ ".)\n\n\n"

intermGoals constructors rrules =
  let (p,d) = foldl(\(p1,d1) rr ->
                      let d2 = depth rr in
                      if d2 > d1 then (ip rr, d2)
                      else (p1,d1)
                    ) (Var "", -1) rrules in
  if d < 2 then []
  else constructGoals constructors (d-1) p

depth rr = depthIP 0 $ ip rr
  
depthIP n (Cons id []) = 0
depthIP n (Cons id lp) = maximum $ map (depthIP (n+1)) lp
depthIP n (LAV _ p)    = depthIP n p
depthIP n (Var _)      = n + 1

constructGoals csts d p =
  if d == 1 then [] else
  constructGoal csts "var1" d p : constructGoals csts (d-1) p 

constructGoal csts v d (LAV _ p)    = constructGoal csts v d p
constructGoal csts v d (Var _)      = VarG v typeofExpr True
constructGoal csts v d (Cons id lp) =
  let c = getC csts id in
  if d == 1 then (VarG v (typ c) True)
  else 
    let (_,_,lg) =
          (foldr
           (\p (v1, n, lg1) -> case p of
               (Var _) -> ((suc v1), (n-1),
                           ((VarG v ((sub c) !! (n-1)) False):lg1))
               _  -> (v1, n-1,
                      (constructGoal csts (v1) (d-1) p):lg1)
           ) (v, length lp, []) lp)
    in ConsG id lg


suc s = let i = read (drop 3 s) :: Int in
  (take 3 s) ++ (show (i+1))

showGoalPat p = toStringp $ toPat p
toPat (ConsG id lg) = (Cons id (map toPat lg))
toPat (VarG s t _)    = Var (s ++ ":" ++ t)

showLemmaPat p = toStringp $ toLemma p
toLemma (ConsG id lg) = (Cons id (map toLemma lg))
toLemma (VarG s t _)  = (Var s)

instance Show Tactics where
  show AUTO = "auto"
  show SI = "SI"
  show CA = "CA"
  show TC = "TC"
  show IP = "IP"
  show CS = "CS"
  show RD = "RD"

instance Show Goal where
  show (EQU mod put rput gp) =
    "(goal PUT-" ++ show mod ++ " |- eq " ++
    put  ++ "(" ++ showGoalPat gp ++"," ++
    rput ++ "(" ++ showGoalPat gp ++ ")) = " ++
    showGoalPat gp ++ " ;)\n"

  show (CEQ mod put rput gp gpIH) =
    "(goal PUT-" ++ show mod ++ " |- ceq " ++
    put  ++ "(" ++ showGoalPat gp ++ "," ++
    rput ++ "(" ++ showGoalPat gp ++ ")) = " ++
    showGoalPat gp ++"\n\tif " ++
    put  ++ "(" ++ showGoalPat gpIH ++ "," ++
    rput ++ "(" ++ showGoalPat gpIH ++ ")) = " ++
    showGoalPat gpIH ++ " ;)\n"

  show (PR mod t) =
    "(goal PUT-" ++ show (mod) ++ " |- eq pr(S:" ++
    t ++ ",S:" ++ t ++ ") = S:" ++ t ++ " ;)\n"


showLemma (EQU _ put rput gp) =
  " eq " ++ put ++ "(" ++
  showLemmaPat gp ++"," ++ rput ++ "(" ++
  showLemmaPat gp ++ ")) = " ++
  showLemmaPat gp ++ " .\nendfm)\n\n"

showLemma (CEQ _ put rput gp gpIH) =
  " ceq " ++ put ++ "(" ++
  showLemmaPat gp ++"," ++ rput ++ "(" ++
  showLemmaPat gp ++ ")) = " ++
  showLemmaPat gp ++ "\n\tif " ++ put ++ "(" ++
  showLemmaPat gpIH ++"," ++ rput ++ "(" ++
  showLemmaPat gpIH ++ ")) = " ++
  showLemmaPat gpIH ++ " .\nendfm)\n\n"

showLemma (PR _ _)= " eq pr(S,S) = S .\nendfm)\n\n"












{-
     ___
    (p,q)___/)
     \wwwwwww)
      \wwwww)
        | |
        ^ ^
                       
      ___        ----  _
     (°v°)      \ o  \/ )
      | |       /    /\_)   
      ^ ^        ----
   __      _
  /  \>  </ \
 (    \></   ) 
 (  0  \/    )
  \         /
   \       /
    \     /
     \   /
      \ /
      / \ 
     /   \
    / / \ \
   /_/   \_\
    

-}
