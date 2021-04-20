all: *.hs
	ghc -outputdir /tmp/ -dynamic -O2 main.hs

test: *.hs
	ghc -outputdir /tmp/ -dynamic -O2 tests.hs
	./tests

.PHONY: clean
clean:
	rm -fv *.o *.hi main tests

.PHONY: watch
watch:
	fd -e hs | entr make
