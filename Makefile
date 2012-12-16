libstemmer_c/LICENSE: libstemmer_c/Makefile
	cp "LICENSE.libstemmer_c" "$@"

libstemmer_c/Makefile: libstemmer_c.tgz
	tar -xzvf "$<"

libstemmer_c.tgz:
	wget "http://snowball.tartarus.org/dist/libstemmer_c.tgz"
