.PHONY: all run bin/fed bin/fed-stable
all: bin/fed

run: bin/fed
	./bin/fed

bin/fed: lib/termio.so lib/shell.so
	sbcl --noinform --non-interactive --eval '(load "load.lisp")' --eval '(sb-ext:save-lisp-and-die "$@" :toplevel '\''fed::main :executable t)'

bin/fed-stable: lib/termio.so lib/shell.so
	sbcl --noinform --non-interactive --eval '(load "load.lisp")' --eval '(sb-ext:save-lisp-and-die "$@" :toplevel '\''fed::main :executable t)'

lib/termio.so: fed/termio.c
	cc -o $@ -shared -fPIC $^

lib/shell.so: fsh/shell.c
	cc -o $@ -shared -fPIC $^

f0/forth.o: f0/forth.s
	as -o $@ $^

f0/forth: f0/forth.o
	ld -o $@ $^
