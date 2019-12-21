main: build

TRIVIAL_GRAY_STREAMS_COMMIT = ebd59b1afed03b9dc8544320f8f432fdf92ab010

trivial-gray-streams.tar.gz:
	curl -L https://github.com/trivial-gray-streams/trivial-gray-streams/archive/$(TRIVIAL_GRAY_STREAMS_COMMIT).tar.gz -o trivial-gray-streams.tar.gz

trivial-gray-streams: trivial-gray-streams.tar.gz
	tar zxf trivial-gray-streams.tar.gz
	mv trivial-gray-streams-$(TRIVIAL_GRAY_STREAMS_COMMIT) trivial-gray-streams

bin/getac: trivial-gray-streams
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
	rm -f bin/getac trivial-gray-streams.tar.gz
	rm -rf trivial-gray-streams
