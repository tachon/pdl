all:
	ghc AST.hs Example.hs SyntacticConstraints.hs Totality.hs ViewDetermination.hs SourceStability.hs Main.hs -o exe

clean:
	rm *exe *.hi *.o *~
