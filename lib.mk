lib/%: %.tgz | lib
	tar -xzvf "$<" -C "$|"

%.tgz:
	curl -O "http://snowball.tartarus.org/dist/$@"

lib:
	mkdir "$@"
