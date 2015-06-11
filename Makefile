all:
	ghc AST.hs PatExhaustiveness.hs ViewDetermination.hs ValidityChecking.hs Main.hs -o exe

ast:
	ghc -Wall -W AST.hs

pate:
	ghc AST.hs PatExhaustiveness.hs

clean:
	rm *exe *.hi *.o *~
