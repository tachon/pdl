module AST where

import qualified Data.Set as Set
import qualified Data.List as List
import Debug.Trace
 
debug = False

mytrace msg val  = if debug then trace msg val
                  else val

mytrace2 msg val = if debug then trace (msg ++ (show val)) val
                   else val

myError msg b   = if b then b else trace msg False


------------------------------------------------------------------

type ID = String

type Tip = String

data C = C { idt :: ID,
             typ :: Tip,
             sub :: [Tip] 
           } deriving (Show, Eq, Ord)

------------------------------------------------------------------

data Pat = Cons ID [Pat]        
         | LAV String Pat  --lookahead variable
         | Var String deriving (Eq)

data Expr = CE ID [Expr]
          | VarE String
          | Fun String String String deriving (Eq)
          --     idFun x y

data Rule = Rule { name :: String,
                   ps :: Pat,
                   pv :: Pat,
                   xpr :: Expr
                 } deriving (Eq)


------------------------------------------------------------------

data RExpr = CRE ID [RExpr]
           | VarRE String
           | FunRE String String deriving (Eq)

data ReversedRule = RRule { rn :: String,
                            ip :: Pat,
                            op :: RExpr
                          } deriving (Eq)


------------------------------------------------------------------

data Fun = F { fName :: String,
               tps   :: Tip,
               tpv   :: Tip,
               txp   :: Tip
             } deriving (Show, Eq, Ord)

data RFun = RF { rfName :: String,
                 tip   :: Tip,
                 top   :: Tip
               } deriving (Show, Eq, Ord)


------------------------------------------------------------------


data Input = I { funs :: [Fun],
                 ctrs :: [C],
                 rls  :: [Rule]
               }
  


getC cl id = case List.find ((==) id . idt) cl of
   Just c -> c
   Nothing -> trace ("Constructor "++ show id
                      ++ " does not exist")
               C {idt="", typ="", sub = []}

consOfType cl t = filter (\c -> typ c == t ) cl

arity c = length $ sub c

getArgFun (VarE _)      = Nothing
getArgFun (Fun _ a1 a2) = Just (a1, a2)
getArgFun (CE i xpl)    =
  foldl (\a xp -> case getArgFun xp of
            Nothing -> a
            Just a1 -> Just a1
        ) Nothing xpl
    

instance Show Pat where
  show (Cons i pl)   = "C " ++ show i ++ " " ++ show pl
  show (LAV v p)     = show v ++ " @ " ++ show p
  show (Var v)       = show v

instance Show Expr where
  show (CE i xpl)    = "C " ++ show i ++ " " ++ show xpl
  show (VarE v)      = show v
  show (Fun f v1 v2) = show f ++" "++ show v1 ++" "++ show v2

instance Show RExpr where
  show (CRE i xpl)   = "C " ++ show i ++ " " ++ show xpl
  show (VarRE v)     = show v
  show (FunRE f v)   = show f ++ " " ++ show v 

instance Show ReversedRule where
  show (RRule {rn=s, ip=p, op=re}) =
    show s ++ " " ++ show p ++ "  =  " ++ show re ++ "\n"

instance Show Rule where
  show (Rule {name=s, ps=src, pv=vew, xpr=xp}) =
    show s   ++ " (" ++
    show src ++ ")   (" ++
    show vew ++ ")  =  " ++
    show xp  ++ "\n"


class PatExpr a where
  goodNumberSub :: [C] -> a -> Bool
  getVariables  :: a -> Set.Set String

instance PatExpr Pat where
  getVariables (LAV s p)     = Set.insert s (getVariables p)
  getVariables (Var s)       = Set.singleton s
  getVariables (Cons _ vp)   =
    foldl (\acc p -> Set.union acc (getVariables p)) Set.empty vp

  goodNumberSub _    (Var _)     = True
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
  getVariables (Fun _ s1 s2) = Set.insert s1 $ Set.singleton s2
  getVariables (VarE s)      = Set.singleton s
  getVariables (CE _ xp)     =
    foldl (\acc p -> Set.union acc (getVariables p)) Set.empty xp
    
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


instance PatExpr RExpr where
  getVariables (FunRE _ s) = Set.singleton s
  getVariables (VarRE s)   = Set.singleton s
  getVariables (CRE _ xp)  = 
    foldl (\acc p -> Set.union acc (getVariables p)) Set.empty xp
    
  goodNumberSub _ (VarRE _)     = True
  goodNumberSub _ (FunRE _ _)   = True
  goodNumberSub cons (CRE i vp) =    
    let c    = getC cons i
        lsub = length $ sub c
        lvp  = length vp
    in (myError ("Constructor " ++ show i ++
                 " should be applicate to " ++ show lsub ++
                 " patterns but here is applicate to " ++ 
                 show lvp ++ " patterns")
        (lvp == lsub)
       ) && (and $ map (goodNumberSub cons) vp)
