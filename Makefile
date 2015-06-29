all:
	ghc AST.hs Example.hs PatExhaustiveness.hs ViewDetermination.hs SourceStability.hs ValidityChecking.hs Main.hs -o exe

clean:
	rm *exe *.hi *.o *~
