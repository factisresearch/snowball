fixtures := $(patsubst %.xz,%,$(wildcard share/*.txt.xz))

.PHONY: all
all: libstemmer_c/LICENSE

libstemmer_c/LICENSE: libstemmer_c/Makefile
	cp "LICENSE.libstemmer_c" "$@"

libstemmer_c/Makefile: libstemmer_c.tgz
	tar -xzvf "$<"

libstemmer_c.tgz:
	wget "http://snowball.tartarus.org/dist/libstemmer_c.tgz"

$(fixtures): %: %.xz
	unxz -k "$<"

test/snowball-fixtures: test/snowball-fixtures.hs
	ghc -O -threaded -rtsopts "$<"

.PHONY: test
test: test/snowball-fixtures $(fixtures)
	./test/snowball-fixtures $(fixtures) +RTS -N

test/stemmer-fixtures: test/stemmer-fixtures.hs
	ghc -O -threaded -rtsopts "$<"

.PHONY: stemmer
stemmer: test/stemmer-fixtures $(fixtures)
	./test/stemmer-fixtures $(fixtures) +RTS -N
