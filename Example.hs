module Example where
import AST

typeofP1 = ToP {
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

---------------------------------------------------------------------
--mark



typeofP = ToP {
  typeofPS   = "SsList",
  typeofPV   = "VsList",
  typeofExpr = "SsList"
}

cex6 = [ 
  C { idt="sscores", typ="Sscores", sub = ["Mark","Mark","Mark"]},
  C { idt="ssCons",  typ="SsList",  sub = ["Sscores", "SsList"]},
  C { idt="ssEmpty", typ="SsList",  sub = []},

  C { idt="vscores", typ="Vscores", sub = ["Mark", "Mark"]},
  C { idt="vsCons",  typ="VsList",  sub = ["Vscores", "VsList"]},
  C { idt="vsEmpty", typ="VsList",  sub = []},

  C { idt="A", typ="Mark", sub = []},
  C { idt="B", typ="Mark", sub = []},
  C { idt="C", typ="Mark", sub = []},
  C { idt="D", typ="Mark", sub = []}
  ]        


{-
goodMark   [ ] 		 	[ ]		=   [ ]
goodMark   ss @ [ ] 		{xv, yv} : vs	=   {xv, A, yv} : goodMark ss vs
goodMark   {xs, A, zs} : ss 	vs @ [ ]	=   goodMark ss vs
goodMark   {xs, A, zs} : ss 	{xv, yv} : vs	=   {xv, A, yv} : goodMark ss vs
goodMark   {xs, B, zs} : ss 	vs		=   {xs, B, zs} : goodMark ss vs
goodMark   {xs, C, zs} : ss 	vs	        =   {xs, C, zs} : goodMark ss vs
goodMark   {xs, D, zs} : ss 	vs	        =   {xs, D, zs} : goodMark ss vs
-}

rex6 = [
  Rule {name="goodMark",
        ps  =(Cons "ssEmpty" []),
        pv  =(Cons "vsEmpty" []),
        xpr =(CE "ssEmpty" [])
       },    

  Rule {name="goodMark",
        ps  =(LAV "ss"(Cons "ssEmpty" [])),        
        pv  =(Cons "vsCons"
              [Cons "vscores" [Var "xv", Var "yv"],
               Var "vs"]),
        xpr =(CE "ssCons"
              [CE "sscores" [VarE "xv", CE "A" [], VarE "yv"],
               Fun "goodMark" "ss" "vs"])
       },    

  Rule {name="goodMark",
        ps  =(Cons "ssCons"
              [Cons "sscores" [Var "xs", Cons "A" [], Var "zs"],
               Var "ss"]),
        pv  =(LAV "vs"(Cons "vsEmpty" [])),
        xpr =(Fun "goodMark" "ss" "vs")
       },    

  Rule {name="goodMark",
        ps  =(Cons "ssCons"
              [Cons "sscores" [Var "xs", Cons "A" [], Var "zs"],
               Var "ss"]),     
        pv  =(Cons "vsCons"
              [Cons "vscores" [Var "xv", Var "yv"],
               Var "vs"]),
        xpr =(CE "ssCons"
              [CE "sscores" [VarE "xv", CE "A" [], VarE "yv"],
               Fun "goodMark" "ss" "vs"])},

  Rule {name="goodMark",
        ps  =(Cons "ssCons"
              [Cons "sscores" [Var "xs", Cons "B" [], Var "zs"],
               Var "ss"]),     
        pv  =(Var "vs"),
        xpr =(CE "ssCons"
              [CE "sscores" [VarE "xs", CE "B" [], VarE "zs"],
               Fun "goodMark" "ss" "vs"])},

  Rule {name="goodMark",
        ps  =(Cons "ssCons"
              [Cons "sscores" [Var "xs", Cons "C" [], Var "zs"],
               Var "ss"]),     
        pv  =(Var "vs"),
        xpr =(CE "ssCons"
              [CE "sscores" [VarE "xs", CE "C" [], VarE "zs"],
               Fun "goodMark" "ss" "vs"])},

  Rule {name="goodMark",
        ps  =(Cons "ssCons"
              [Cons "sscores" [Var "xs", Cons "D" [], Var "zs"],
               Var "ss"]),     
        pv  =(Var "vs"),
        xpr =(CE "ssCons"
              [CE "sscores" [VarE "xs", CE "D" [], VarE "zs"],
               Fun "goodMark" "ss" "vs"])}
  ]

{-
   { [A,D,B],        
     [C,A,B],    ->  {[C,B],
     [A,A,D],    <-   [A,D],
     [A,B,D] }        [A,D]}

keep students who got A in second discipline

-}



















































---------------------------------------------------------------------
--people

typeofP7 = ToP {
  typeofPS   = "People",
  typeofPV   = "FTokyo",
  typeofExpr = "People"
}



{-
rex3 = [Rule {name="people",
              ps  =(Cons "people" 
                    [Cons "consP"
                     [Cons "person"
                      [Cons "name" [Var "n1"],
                       Cons "city"
                       [Cons "consS"
                        [Cons "t" [] ,
                         Cons "consS"
                         [Cons "o" [] ,
                          Cons "consS"
                          [Cons "k" [] ,
                           Cons "consS"
                           [Cons "y" [] ,
                            Cons "sgltS"
                            [Cons "o" []
                            ]]]]]]]
                     ,Var "ps"]]
                   ),
              
              pv =(Cons "fromT"
                   [Cons "ConsN"
                    [Cons "name" [Var "n2"]
                    , Var "ns"]]
                  ),

                xpr =(Fun "sameName" "n1" "n2" )
-}



  --Cheat a bit : define constructors in function of the source
  ---------"Tokyo"   -> Tokyo
  ---------"Kiel"    -> Kiel

  --Cheat a lot : define constructors in function of the rules
  --------- "Tokyo"  -> Tokyo
  --------- "Kiel"   -> NotTokyo ("Kiel")

{-cex6 = [
  C { idt="a", typ="Char", sub = []},
         C { idt="b", typ="Char", sub = []},
         C { idt="c", typ="Char", sub = []},
         C { idt="d", typ="Char", sub = []},
         C { idt="e", typ="Char", sub = []},
         C { idt="f", typ="Char", sub = []},
         C { idt="g", typ="Char", sub = []},
         C { idt="h", typ="Char", sub = []},
         C { idt="i", typ="Char", sub = []},
         C { idt="j", typ="Char", sub = []},
         C { idt="k", typ="Char", sub = []},
         C { idt="l", typ="Char", sub = []},
         C { idt="m", typ="Char", sub = []},
         C { idt="n", typ="Char", sub = []},
         C { idt="o", typ="Char", sub = []},
         C { idt="p", typ="Char", sub = []},
         C { idt="q", typ="Char", sub = []},
         C { idt="r", typ="Char", sub = []},
         C { idt="s", typ="Char", sub = []},
         C { idt="t", typ="Char", sub = []},
         C { idt="u", typ="Char", sub = []},
         C { idt="v", typ="Char", sub = []},
         C { idt="w", typ="Char", sub = []},
         C { idt="x", typ="Char", sub = []},
         C { idt="y", typ="Char", sub = []},
         C { idt="z", typ="Char", sub = []},
         
         C { idt="sgltS",  typ="String", sub = ["Char"]},
         C { idt="consS",  typ="String", sub = ["Char", "String"]},


  C { idt="sebastian", typ="Name", sub = []},
  C { idt="hugo",      typ="Name", sub = []},
  C { idt="zhenjiang", typ="Name", sub = []},

  C { idt="nothing",   typ="MaybeName", sub = []},
  C { idt="just",      typ="MaybeName", sub = []},
  C { idt="alignN",    typ="Name", sub = ["Name", "Name", "Name"]},
  
  
  C { idt="tokyo",     typ="City", sub = []},
  C { idt="notTokyo",  typ="City", sub = ["String"]},
  
  C { idt="emptyN",    typ="Names", sub = []},
  C { idt="consN",     typ="Names", sub = ["Name","Names"]},

  C { idt="person",    typ="Person",  sub = ["String","City"]},
  C { idt="emptyP",    typ="Persons", sub = []},
  C { idt="consP",     typ="Persons", sub = ["Person","Persons"]},
  
  C { idt="people",    typ="People", sub = ["Persons"]},
  C { idt="fromT",     typ="FTokyo", sub = ["Names"]}  
  ]

rex3 = [
  Rule {name="general",
        ps  =(Cons "people" [Var "ps"]),
        
        pv  =(Cons "fromT" [Var "ns"]),
              
        xpr =(CE "people" [Fun "AreFromTokyo" "ps" "ns"])
       },


  Rule {name="AreFromTokyo",
        ps  =(Cons "consP" [Cons "person" [Var "n1",
                                           Cons "tokyo" []],
                            Var "ps"]
             ),
              
        pv  =(Cons "consN" [Var "n2", Var "ns"]),
        
        xpr =(Cons "consP" [
         
                 Fun "match" "n1" "n2",
                 Fun "AreFromTokyo" "ps" "ns"])
       }        
  ]


match    : name in source and view -> create value
unmatchv : name only in view       -> create value
unmatchs : name only in source     -> delete value

   S                   V
  x_1 <--- match ---> y_1
  x_2\               /y_2
  x_3 `----unmatchs-( y_3
  x_4                \y_4


   S    V  
  [1   [3 
  ,3   ,4
  ,5]  ,6]

A-match    -> [3]
B-unmatchs -> [1,5]
C-unmatchv -> [4,6]

A U B U C = S U V

A N B = ø
A N C = ø
B N C = ø



alignS s (v : vs) -> (match s v)? match : alignS s vs ;
alignS s ([] @ v) -> unmatchS s v


alignV (s : ss) v -> (match s v)? match : alignV ss v ;
alignS ([] @ s) v -> unmatchV s v

test (s:ss) (v:vs) -> C1 [
                          C2 [alignS s (v:vs), alignV (s:ss) v]
                          , test ss vs
                         ]

case 1                    unmatchs      ,    match
case 2                    









-}


