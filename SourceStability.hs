module SourceStability where

import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import Debug.Trace
import Data.Char

import AST
import Example
import Totality
import SyntacticConstraints

data Tactics = SI | CA | TC | IP | CS | RD | AUTO 

type Ind = Bool

data GoalPat = ConsG ID [GoalPat]
             | VarG String Tip Ind deriving(Eq, Ord)


data Goal = EQU  Int String String GoalPat
          | CEQ Int String String GoalPat GoalPat
          | PR  Int Tip
            
debug = True

writeCITPFile funs csts rules rrules =
  let (s,n) = foldl (\(s1,n1) f ->
                      let (s2,n2) = writeGoals csts rrules
                                    f (reversedFun f)
                      in (s1 ++ "\n\n" ++ s2, n1+n2)
                    ) ("",0) funs
  in (writePUTmod funs csts rules rrules ++ s, n)
  
writePUTmod funs constructors rules rrules  =
  "(fmod PUT-0 is\n" --PUT to be changed
  ++ " sorts " ++ allTypes constructors ++ " .\n---\n"
  ++ writeConst constructors ++ "---\n"
  ++ writeVars (getAllVars funs constructors rules rrules)
  ++ "---\n"
  ++ writeRules funs rules ++ "\n"
  ++ writeRRules (reversedFuns funs) rrules ++ "\n"
  ++ writeSSProperty funs
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

getAllVars funs cons rules rrules =
  Map.foldWithKey
  (\f rofs map ->
    foldl (
      \map r -> 
      let (_,map1) = patWellTyped cons (tps f) (ps r) Map.empty
          (_,map2) = patWellTyped cons (tpv f) (pv r) map1
      in Map.union map map2
      ) Map.empty rofs
  ) Map.empty (rulesOfFuns funs rules)
  `Map.union`
  Map.foldWithKey
  (\rf rrofs map ->
    foldl (
      \map r -> 
      let (_,map1) = patWellTyped cons (tip rf) (ip r) map 
      in Map.union map map1
      ) Map.empty rrules 
    `Map.union`
    (Map.fromList [("x1",(tip rf)), ("y1",(tip rf))])
  ) Map.empty (rrulesOfFuns (reversedFuns funs) rrules)
    
writeVars vars =
  Map.foldWithKey
  (\t lv s ->
    " vars " ++ unwords lv ++ " : " ++ t ++ " .\n" ++ s
  ) "" (Map.foldWithKey
  (\v t m -> Map.insertWith (++) t [v] m) Map.empty vars)

writeRules funs rules =
  foldl(\s f ->
         " op " ++ (fName f) ++
         " : " ++  (tps f) ++ " " ++ (tpv f) ++
         " -> " ++ (txp f) ++ " .\n" ++
         (unlines $ map
          (\r ->
            let env = getLAV r in
            " eq " ++ (name r) ++ "(" 
            ++ (toStringp $ ps r) ++ ","
            ++ (toStringp $ pv r) ++ ") = "
            ++ (toStringe env (xpr r)) ++ " .")
          (filter ((fName f ==) . name) rules))
       ) "" funs
  
writeRRules rfuns rrules =
  foldl(\s rf ->
         " op " ++ (rfName rf) ++
         " : " ++  (tip rf) ++
         " -> " ++ (top rf) ++ " .\n" ++
         (unlines $ map
          (\rr -> " eq " ++ (rn rr) ++ "(" 
                  ++ (toStringp $ ip rr) ++ ") = "
                  ++ (toStringre $ op rr) ++ " .")
          (filter ((rfName rf ==) . rn) rrules))
       ) "" rfuns

writeSSProperty funs =
  foldl (\s f ->
          " op pr : " ++
          (txp f) ++ " " ++
          (txp f) ++ " -> " ++
          (txp f) ++ " .\n" ++
          " eq pr(x1,y1) = " ++ (fName f) ++
          "(x1," ++ ('R' : (fName f)) ++ "(y1)) .\n"
        ) "" funs

writeGoals csts rrules f rf =
  let lg = intermGoals csts f rrules in
  if null lg then ((lastGoals 0 f rf),0) else
    let (s1, n1) =
          foldl (
            \(s, n) (gp,gpIH) ->
            let g = CEQ n (fName f) (rfName rf) gp gpIH
            in (s ++ show g 
            ++ writeInd gp
            ++ (applyTactics $ findTactics csts gp)
            ++ createModule g (n+1), n+1)
            ) ("", 0) lg 
    in (s1 ++ (lastGoals n1 f rf), n1)

lastGoals n f rf =
  let gp = VarG "S" (tps f) True
      g1 = EQU n (fName f) (rfName rf) gp 
      g2 = PR (n+1) (tps f)
  in
  show g1 ++
  writeInd gp ++
  applyTactics [SI, IP] ++
  createModule g1 (n+1) ++
  show g2 ++ applyTactics [TC, IP]
  
findTactics csts (ConsG id lg) =
  concat $ map (findTactics csts) lg
  
findTactics csts (VarG _ t b) =
  if not b then []
  else let n = List.length $ consOfType csts t in
  SI : (concat $ take n $ repeat [TC,IP,TC])

writeInd g =
  (Set.foldl (\s (v,t) -> s ++ " " ++ v ++ ":" ++ t) 
  "(set ind on" (findIndVars g ))
  ++ " .)\n"

createModule g n =
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

writeInclude n = " inc PUT-" ++ show n ++ " .\n----\n"


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

depth rr = depthIP 0 $ ip rr
  
depthIP n (Cons id []) = 0
depthIP n (Cons id lp) = maximum $ map (depthIP (n+1)) lp
depthIP n (LAV _ p)    = depthIP n p
depthIP n (Var _)      = n + 1

initV  = "var1"
incr s = let i = read (drop 3 s) :: Int
         in (take 3 s) ++ (show (i+1))
suc (n,m) s =
  let lmp = take (n+1) $ iterate (m*) 1
      un  = read (drop 3 s) :: Integer
      res = (sum lmp) + (un - (sum $ init lmp)) * m 
  in  "var" ++ show res


showGoalPat p       = toStringp $ toPat p
toPat (ConsG id lg) = (Cons id (map toPat lg))
toPat (VarG s t _)  = Var (s ++ ":" ++ t)

showLemmaPat p = toStringp $ toLemma p
toLemma (ConsG id lg) = (Cons id (map toLemma lg))
toLemma (VarG s _ _)  = (Var s)

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


intermGoals constructors f rrules =
  let m  = maximum $ map (length . sub) constructors
      lg = Set.toList $
           foldl
           (\set rr ->
             case (patternIH (op rr)) of
               Nothing -> set
               Just s  ->
                 let pp = parentPattern
                          constructors
                          s
                          (0, toInteger m)
                          initV
                          (txp f)
                          (ip rr)
                 in
                  if isRoot pp then set
                  else
                    Set.insert
                    (pp, VarG s (tps f) False) set
           ) (Set.empty) rrules
  in lg ++
     foldl (\acc (pp, (VarG s _ _)) ->
             (reverse (pToIH constructors f s m pp)) ++ acc
           ) [] lg

isRoot (VarG _ _ _) = True
isRoot _            = False

pToIH csts f s m pp =
  let pp1 = parentPattern
            csts
            s
            (0, toInteger m)
            initV
            (txp f)
            (toLemma pp)
  in case pp1 of
    (VarG _ _ _) -> []
    pp1          ->
      (pp1, VarG s (tps f) False) :
      (pToIH csts f s m pp1)

  
patternIH (VarRE _)   = Nothing
patternIH (FunRE _ s) = Just s
patternIH (CRE id lp) =
  let l = filter (\p -> case p of
                     Nothing -> False
                     Just _ -> True)
          $ map patternIH lp
  in if null l then Nothing else head l

parentPattern _ s1 _ svar t (Var s) =
  if s == s1 then VarG s t False else VarG svar t False
parentPattern csts s1 (n,m) svar t (Cons id lp) =
  if and $ map (\p -> case p of
                   Var _ -> True
                   _     -> False) lp
  then
    VarG svar t True
  else
    let tsub = (sub $ getC csts id)
        sv1  = suc (n,m) svar in
    ConsG id (zipWith3 (parentPattern csts s1 (n+1,m))
              (iterate incr sv1) tsub lp)


reversedFuns funs =
  map (\f ->
        RF {
          rfName='R': (fName f) ,
          tip   =tps f,
          top   =tpv f
          }) funs

reversedFun f =
  RF { rfName='R': (fName f) ,
       tip   =tps f,
       top   =tpv f
     }
  
rrulesOfFuns rfuns rrules =
  foldl (\map f ->
          Map.insert f
          (filter ((rfName f ==) . rn) rrules)
          map) Map.empty rfuns



{-
for each RRules (not BC):
- I make a map of (GoalPat, GoalPat) * INT
- fst is the direct parent of pattern
  - parent : pattern where leaf cons is replaced by a var (ind = True)
  - cons are leaf when all his sub are var or no sub (arity=0)
- snd is the pattern HI
- (use an upper bound (nb rr) to know nb of tactics)

I construct from fst to root :
with each father -> son
-}























{-   _
  __( (
 |_    \____
 |_
 |_     ____
 |_____/



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

   ____
  (-__-)
  *-||-*
   _/\_

   ____              (-__-)                
  (-__-)           .___||
    ||<@========>        >
   _/\_

-}
