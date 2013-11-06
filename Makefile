.PHONY: all
all: lib/libstemmer_c

.PHONY: vocabulary
vocabulary:
	$(MAKE) -C "test" "$@" LANG=C
	$(MAKE) -C "test" "$@" FLAGS=-fstemmer LANG=C

.PHONY: parallel
parallel:
	$(MAKE) -C "test" "$@"

include lib.mk
