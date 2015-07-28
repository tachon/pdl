all:
	ghc AST.hs Example.hs PatExhaustiveness.hs SyntacticConstraints.hs ViewDetermination.hs SourceStability.hs Main.hs -o exe

clean:
	rm *exe *.hi *.o *~
