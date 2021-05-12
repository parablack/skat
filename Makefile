all: skat-backend/**.hs
	cd skat-backend && ghc -outputdir /tmp/ -dynamic -O2 main.hs

static: skat-backend/**.hs
	cd skat-backend && ghc -outputdir /tmp/ -O2 main.hs

test: skat-backend/**.hs
	cd skat-backend && ghc -outputdir /tmp/ -dynamic -O2 tests.hs
	./tests

.PHONY: clean
clean:
	rm -fv skat-backend/*.o skat-backend/*.hi skat-backend/main skat-backend/tests

.PHONY: watch
watch:
	fd -e hs | entr make

frontend: skat-frontend/src/*.tsx
	cd skat-frontend && yarn build

deploy: all frontend
	scp -r skat-backend/* bpara:/var/www/skat/
	scp -r skat-frontend/build/* bpara:/var/www/skat/frontend/
	ssh -f bpara 'systemctl restart skat'
	# cabal install --only-dependencies
