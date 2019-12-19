main: build

trivial-gray-streams-master.tar.gz:
	curl -L https://github.com/trivial-gray-streams/trivial-gray-streams/archive/master.tar.gz -o trivial-gray-streams-master.tar.gz

trivial-gray-streams-master: trivial-gray-streams-master.tar.gz
	tar zxf trivial-gray-streams-master.tar.gz

bin/getac: trivial-gray-streams-master
	mkdir -p bin
	sbcl --eval '(require :asdf)' \
		--eval '(push #P"trivial-gray-streams-master/" asdf:*central-registry*)' \
		--eval '(push #P"./" asdf:*central-registry*)' \
		--eval '(asdf:load-system :getac)' \
		--eval '(sb-ext:save-lisp-and-die "bin/getac" :toplevel (function getac/cli:main) :executable t :save-runtime-options t)'

.PHONY: build install uninstall clean

build: bin/getac

install:
	cp bin/getac /usr/local/bin

uninstall:
	rm /usr/local/bin/getac

clean:
	rm -f bin/getac trivial-gray-streams-master.tar.gz
	rm -rf trivial-gray-streams-master
