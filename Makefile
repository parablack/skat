all: *.hs
	ghc -outputdir /tmp/ -dynamic -O2 main.hs

static: *.hs
	ghc -outputdir /tmp/ -O2 main.hs

test: *.hs
	ghc -outputdir /tmp/ -dynamic -O2 tests.hs
	./tests

.PHONY: clean
clean:
	rm -fv *.o *.hi main tests

.PHONY: watch
watch:
	fd -e hs | entr make

frontend: skat-frontend/src/*.tsx
	cd skat-frontend && yarn build

deploy: all frontend
	scp *.hs bpara:/var/www/skat/
	scp Skat.cabal bpara:/var/www/skat/
	scp -r skat-frontend/build bpara:/var/www/skat/frontend
	ssh -f bpara 'systemctl restart skat'
	# cabal install --only-dependencies