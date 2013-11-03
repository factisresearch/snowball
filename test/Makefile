.NOTPARALLEL:

.PHONY: snowball
snowball: | dist
	cabal configure
	cabal build

.PHONY: stemmer
stemmer: | dist
	cabal configure -fstemmer
	cabal build

dist:
	cabal install --only-dependencies
	cabal install --only-dependencies -fstemmer
