module Example where
import AST

typeofPS   = "Tag List"
typeofPV   = "Val List"
typeofExpr = "Tag List"



cex = [ C { idt="[]t", typ="Tag List", sub = []},              
        C { idt="[]v", typ="Val List", sub = []},
        
        C { idt=":t", typ="Tag List", sub = ["Tag", "Tag List"]},
        C { idt=":v", typ="Val List", sub = ["Val", "Val List"]}, 

        C { idt="A",  typ="Tag",  sub = ["Val"]},        
        C { idt="B",  typ="Tag",  sub = ["Val"]}]        
      


rex = [Rule {name="putAs",
             ps  =(Cons "[]t" []), pv =(Cons "[]v" []),
             xpr =(CE "[]t" [])},
     
       Rule {name="putAs",
             ps  =(LAV "ss" (Cons "[]t" [])),
             pv  =(Cons ":v" [(Var "v"), (Var "vs")]),
             xpr =(CE ":t" [CE "A" [(VarE "v")],
                            (Fun "putAs" "ss" "vs")])},
       
       Rule {name="putAs",
             ps  =(Cons ":t" [(Cons "A" [(Var "a")]), (Var "ss")]),
             pv  =(LAV "vs" (Cons "[]v" [])),
             xpr =(Fun "putAs" "ss" "vs")},
       
       Rule {name="putAs",
             ps  =(Cons ":t" [(Cons "A" [(Var "a")]), (Var "ss")]),
             pv  =(Cons ":v" [(Var "v"), (Var "vs")]),
             xpr =(CE ":t" [CE "A" [(VarE "v")],
                         (Fun "putAs" "ss" "vs")])},
       
       Rule {name="putAs",
             ps  =(Cons ":t" [(Cons "B" [(Var "b")]), (Var "ss")]),
             pv  =(Var "vs"),
             xpr =(CE ":t" [CE "B" [(VarE "b")],
                         (Fun "putAs" "ss" "vs")])}       
      ]
      
rex1 = [Rule {name="putAs",
              ps  =(Var "s"),
              pv  =(Var "v"),
              xpr =(Fun "putAs" "s" "v")}
       ]
