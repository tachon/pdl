module Example where
import AST

typeofPS   = "List"
typeofPV   = "List"
typeofExpr = "List"



cex = [ C { idt="[]", typ="List", sub = []},           -- []
        C { idt=":",  typ="List", sub = ["Tag", "List"]}, -- h:l
        C { idt="A",  typ="Tag",  sub = ["Val"]},        -- A
        C { idt="B",  typ="Tag",  sub = ["Val"]}]        -- B
      


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
              ps  =(Var "s"),
              pv  =(Var "v"),
              xpr =(Fun "putAs" "s" "v")}
       ]
