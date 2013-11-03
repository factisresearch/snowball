.PHONY: all
all: lib/libstemmer_c

.PHONY: test
test:
	$(MAKE) snowball
	$(MAKE) stemmer

.PHONY: snowball stemmer
snowball stemmer: lib/snowball_all
	$(MAKE) -C "test" $@
	env LANG=C ./test/dist/build/snowball-voc/snowball-voc

lib/%: %.tgz | lib
	tar -xzvf "$<" -C "$|"

%.tgz:
	curl -O "http://snowball.tartarus.org/dist/$@"

lib:
	mkdir "$@"
