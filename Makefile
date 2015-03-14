.PHONY: greeter

greeter:
	ghc -o build/greeter --make greeter.hs -outputdir build/
