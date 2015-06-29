module Example where
import AST

typeofPS   = "TagList"
typeofPV   = "ValList"
typeofExpr = "TagList"



cex = [ C { idt="emptyT", typ="TagList", sub = []},
        C { idt="emptyV", typ="ValList", sub = []},
        
        C { idt="consT", typ="TagList", sub = ["Tag", "TagList"]},
        C { idt="consV", typ="ValList", sub = ["Val", "ValList"]}, 

        C { idt="A",  typ="Tag",  sub = ["Val"]},        
        C { idt="B",  typ="Tag",  sub = ["Val"]}]        
      


rex = [Rule {name="putAs",
             ps  =(Cons "emptyT" []), pv =(Cons "emptyV" []),
             xpr =(CE "emptyT" [])},
     
       Rule {name="putAs",
             ps  =(LAV "ss" (Cons "emptyT" [])),
             pv  =(Cons "consV" [(Var "v"), (Var "vs")]),
             xpr =(CE "consT" [CE "A" [(VarE "v")],
                            (Fun "putAs" "ss" "vs")])},
       
       Rule {name="putAs",
             ps  =(Cons "consT" [(Cons "A" [(Var "a")]), (Var "ss")]),
             pv  =(LAV "vs" (Cons "emptyV" [])),
             xpr =(Fun "putAs" "ss" "vs")},
       
       Rule {name="putAs",
             ps  =(Cons "consT" [(Cons "A" [(Var "a")]), (Var "ss")]),
             pv  =(Cons "consV" [(Var "v"), (Var "vs")]),
             xpr =(CE "consT" [CE "A" [(VarE "v")],
                         (Fun "putAs" "ss" "vs")])},
       
       Rule {name="putAs",
             ps  =(Cons "consT" [(Cons "B" [(Var "b")]), (Var "ss")]),
             pv  =(Var "vs"),
             xpr =(CE "consT" [CE "B" [(VarE "b")],
                         (Fun "putAs" "ss" "vs")])}       
      ]
      
rex1 = [Rule {name="putAs",
              ps  =(Var "s"),
              pv  =(Var "v"),
              xpr =(Fun "putAs" "s" "v")}
       ]
