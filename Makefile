VOC := ./test/dist/build/snowball-voc/snowball-voc

.PHONY: all
all: lib/libstemmer_c

lib/%: %.tgz | lib
	tar -xzvf "$<" -C "$|"

%.tgz:
	curl -O "http://snowball.tartarus.org/dist/$@"

lib:
	mkdir "$@"

test/dist:
	cabal install ./test --only-dependencies
	cabal install ./test --only-dependencies -fstemmer

.PHONY: test
test: lib/snowball_all | test/dist
	cd test; cabal configure; cabal build
	time $(VOC)
	time env LANG=C $(VOC)
	cd test; cabal configure -fstemmer; cabal build
	time $(VOC)
	time env LANG=C $(VOC)
