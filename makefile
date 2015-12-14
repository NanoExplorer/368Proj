
rli: LispDefs.hs LispStdLib.hs Parsing.hs raskell.hs
	ghc --make raskell.hs -o rli

clean: 
	mv *.hi *.o *~ rli ./trash/
