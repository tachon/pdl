module Example where
import AST

typeofP = ToP {
  typeofPS   = "TagList",
  typeofPV   = "ValList",
  typeofExpr = "TagList"
}
               
cex1 = [ C { idt="emptyT", typ="TagList", sub = []},
        C { idt="emptyV", typ="ValList", sub = []},
        
        C { idt="consT", typ="TagList", sub = ["Tag", "TagList"]},
        C { idt="consV", typ="ValList", sub = ["Val", "ValList"]}, 

        C { idt="TagA",  typ="Tag",  sub = ["Val"]},        
        C { idt="TagB",  typ="Tag",  sub = ["Val"]}]        
      


rex1 = [Rule {name="putAs",
             ps  =(Cons "emptyT" []), pv =(Cons "emptyV" []),
             xpr =(CE "emptyT" [])},
     
       Rule {name="putAs",
             ps  =(LAV "ss" (Cons "emptyT" [])),
             pv  =(Cons "consV" [(Var "v"), (Var "vs")]),
             xpr =(CE "consT" [CE "TagA" [(VarE "v")],
                            (Fun "putAs" "ss" "vs")])},
       
       Rule {name="putAs",
             ps  =(Cons "consT" [(Cons "TagA" [(Var "a")]), (Var "ss")]),
             pv  =(LAV "vs" (Cons "emptyV" [])),
             xpr =(Fun "putAs" "ss" "vs")},
       
       Rule {name="putAs",
             ps  =(Cons "consT" [(Cons "TagA" [(Var "a")]), (Var "ss")]),
             pv  =(Cons "consV" [(Var "v"), (Var "vs")]),
             xpr =(CE "consT" [CE "TagA" [(VarE "v")],
                         (Fun "putAs" "ss" "vs")])},
       
       Rule {name="putAs",
             ps  =(Cons "consT" [(Cons "TagB" [(Var "b")]), (Var "ss")]),
             pv  =(Var "vs"),
             xpr =(CE "consT" [CE "TagB" [(VarE "b")],
                         (Fun "putAs" "ss" "vs")])}       
      ]
      
rex11 = [Rule {name="putAs",
              ps  =(Var "s"),
              pv  =(Var "v"),
              xpr =(Fun "putAs" "s" "v")}
       ]

---------------------------------------------------------------------
--updFirst

typeofP2 = ToP {
  typeofPS   = "Pair",
  typeofPV   = "Val",
  typeofExpr = "Pair"
}

cex2 = [ C { idt="pair", typ="Pair", sub = ["Val", "Val"]}]        

rex2 = [Rule {name="updFst",
             ps  =(Cons "pair" [Var "x", Var "y"]),
             pv  =(Var "v"),
             xpr =(CE "pair" [VarE "v", VarE "y"])}     
      ]



---------------------------------------------------------------------
--updLast

typeofP3 = ToP {
  typeofPS   = "NotNullList",
  typeofPV   = "Val",
  typeofExpr = "NotNullList"
}

cex3 = [ C { idt="singleton", typ="NotNullList", sub = ["Val"]},
         C { idt="cons", typ="NotNullList",
             sub = ["Val", "NotNullList"]}
       ]        

rex3 = [Rule {name="updLast",
             ps  =(Cons "singleton" [Var "s"]),
             pv  =(Var "v"),
             xpr =(CE "singleton" [VarE "v"])},

        Rule {name="updLast",
             ps  =(Cons "cons" [Var "s", Var "ss"]),
             pv  =(Var "v"),
             xpr =(CE "cons" [VarE "s", Fun "updLast" "ss" "v"])}
      ]

---------------------------------------------------------------------
--putSyntactBad


typeofP4 = ToP {
  typeofPS   = "Val",
  typeofPV   = "Val",
  typeofExpr = "Val"
}

cex4 = []        

rex4 = [Rule {name="putSyntactBad",
             ps  =(Var "s"),
             pv  =(Var "v"),
             xpr =(Fun "putSyntactBad" "s" "v")}    
      ]


---------------------------------------------------------------------
--putInvalid


typeofP5 = ToP {
  typeofPS   = "Val",
  typeofPV   = "Val",
  typeofExpr = "Val"
}

cex5 = []        

rex5 = [Rule {name="putInvalid",
             ps  =(Var "s"),
             pv  =(Var "v"),
             xpr =(VarE "s")}    
      ]
