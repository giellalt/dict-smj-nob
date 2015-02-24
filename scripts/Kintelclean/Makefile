Kintelclean: Kintelclean.hs
	ghc -O2 --make $<

test:
	runhaskell Kintelclean.hs test.xml - > results.xml
	diff expected.xml results.xml
