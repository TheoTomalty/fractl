.PHONY: all run bin/fed bin/fed-stable
all: lib/termio.so lib/shell.so bin/fed

run: bin/fed lib/*.so
	./bin/fed

bin/fed:
	sbcl --noinform --non-interactive --eval '(load "load.lisp")' --eval '(sb-ext:save-lisp-and-die "$@" :toplevel '\''fed::main :executable t)'

bin/fed-stable:
	sbcl --noinform --quit --eval '(load "load.lisp")' --eval '(sb-ext:save-lisp-and-die "$@" :toplevel '\''fed::main :executable t)'

lib/termio.so: fed/termio.c
	gcc -o $@ -shared -fPIC $^

lib/shell.so: fsh/shell.c
	gcc -o $@ -shared -fPIC $^
