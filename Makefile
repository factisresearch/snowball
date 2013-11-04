.PHONY: all
all: lib/libstemmer_c

.PHONY: vocabulary
vocabulary:
	$(MAKE) -C "test" "$@"
	$(MAKE) -C "test" "$@" FLAGS=-fstemmer

.PHONY: parallel
parallel:
	$(MAKE) -C "test" "$@"

include lib.mk
