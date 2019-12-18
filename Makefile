main: build

build:
	mkdir -p bin
	sbcl --eval '(require :asdf)' \
		--eval '(asdf:load-system :getac)' \
		--eval '(sb-ext:save-lisp-and-die "bin/getac" \
		                                  :toplevel (function getac/cli:main) \
	                                      :executable t :save-runtime-options t)'

install:
	cp bin/getac /usr/local/bin

uninstall:
	rm /usr/local/bin/getac

clean:
	rm -f bin/getac
