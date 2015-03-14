.PHONY: greeter parser

greeter:
	ghc -o build/greeter --make greeter.hs -outputdir build/

parser:
	ghc -o build/parser --make parser.hs -outputdir build/
