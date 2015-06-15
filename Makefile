all:
	ghc AST.hs Example.hs PatExhaustiveness.hs ViewDetermination.hs ValidityChecking.hs Main.hs -o exe

clean:
	rm *exe *.hi *.o *~
