.PHONY: all
all: lib/libstemmer_c

.PHONY: vocabulary
vocabulary:
	$(MAKE) -C "test" "$@"
	$(MAKE) -C "test" "$@" FLAGS=-fstemmer

include lib.mk
