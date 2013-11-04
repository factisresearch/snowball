.PHONY: all
all: vocabulary

.PHONY: vocabulary
vocabulary: configure | lib/snowball_all
	cabal run "$@"

.PHONY: configure
configure: | cabal.sandbox.config
	cabal configure $(FLAGS)

cabal.sandbox.config:
	cabal sandbox init
	cabal sandbox add-source ../
	cabal install --only-dependencies
	cabal install --only-dependencies -fstemmer

include ../lib.mk
