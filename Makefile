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

test/fixtures: test/fixtures.hs
	ghc -O "$<"

.PHONY: test
test: test/fixtures $(fixtures)
	./test/fixtures $(fixtures)
