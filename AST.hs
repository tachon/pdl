module AST where

--import qualified Data.Set as Set
import qualified Data.List as List
import Debug.Trace
-----------------------------------------------------------
--Constructors used
--same exemple as paper
--to put to another file

debug = False

mytrace msg val  = if debug then trace msg val
                  else val

mytrace2 msg val = if debug then trace (msg ++ (show val)) val
                   else val


myError msg b   = if b then b else trace msg False

type ID = String  --may switch to string

data Tip = List | Tag | Val deriving (Show, Eq, Ord)

data C = C { idt :: ID,
             typ :: Tip,
             sub :: [Tip] 
           } deriving (Show, Eq, Ord)

typeofPS   = List
typeofPV   = List
typeofExpr = List

cl = [ C { idt="[]", typ=List, sub = []},           -- []
       C { idt=":", typ=List, sub = [Tag, List]}, -- h:l
       C { idt="A", typ=Tag, sub = [Val]},        -- A
       C { idt="B", typ=Tag, sub = [Val]}]        -- B
       
getC id = case List.find ((==) id . idt) cl of
   Just c -> c
   Nothing -> trace ("Constructor "++ show id
                      ++ "does not exist")
               C {idt="", typ=List, sub = []}

consOfType t = filter (\c -> typ c == t ) cl

arity c = length $ sub c

----------------------------------------------------------

-- Pattern source
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


rex = [Rule {name="putAs",
             ps  =(Cons "[]" []), pv =(Cons "[]" []), xpr =(CE "[]" [])},
     
       Rule {name="putAs",
             ps  =(LAV "ss" (Cons "[]" [])),
             pv  =(Cons ":" [(Var "v"), (Var "vs")]),
             xpr =(CE ":" [CE "A" [(VarE "v")],
                         (Fun "putAs" "ss" "vs")])},
       
       Rule {name="putAs",
             ps  =(Cons ":" [(Cons "A" [(Var "a")]), (Var "ss")]),
             pv  =(LAV "vs" (Cons "[]" [])),
             xpr =(Fun "putAs" "ss" "vs")},
       
       Rule {name="putAs",
             ps  =(Cons ":" [(Cons "A" [(Var "a")]), (Var "ss")]),
             pv  =(Cons ":" [(Var "v"), (Var "vs")]),
             xpr =(CE ":" [CE "A" [(VarE "v")],
                         (Fun "putAs" "ss" "vs")])},
       
       Rule {name="putAs",
             ps  =(Cons ":" [(Cons "B" [(Var "b")]), (Var "ss")]),
             pv  =(Var "vs"),
             xpr =(CE ":" [CE "B" [(VarE "b")],
                         (Fun "putAs" "ss" "vs")])}       
      ]
      
rex1 = [Rule {name="putAs",
              ps  =(Var "s"), pv =(Cons "[]" []), xpr =(CE "[]" [])},
        
        Rule {name="putAs",
              ps  =(Var "s"),
              pv  =(Cons ":" [(Var "v"), (Var "vs")]),
              xpr =(CE ":" [CE "A" [(VarE "v")],
                          (Fun "putAs" "ss" "vs")])}
       ]
       
       
------------------------------------------------------------------

data RExpr = CRE ID [RExpr]
           | VarRE String
           | FunRE String String deriving (Eq)

data ReversedRule = RRule { rn :: String,
                            ip :: Pat,
                            op :: RExpr
                          } deriving (Eq)

instance Show Pat where
  show (Cons i pl)   = "C " ++ show i ++ " " ++ show pl
  show (LAV v p)     = "("++ show v ++ " @ " ++ show p ++ ")"
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
    show s   ++ " " ++
    show src ++ "   " ++
    show vew ++ "  =  " ++
    show xp  ++ "\n"

getArgFun (VarE _)      = Nothing
getArgFun (Fun _ a1 a2) = Just (a1, a2)
getArgFun (CE i xpl)    =
  foldl (\a xp -> case getArgFun xp of
            Nothing -> a
            Just a1 -> Just a1
        ) Nothing xpl

    

class PatExpr a where
  goodNumberSub :: a -> Bool
