.PHONY: build
default: run

OCAML_LIBS = $(shell ocamlc -where)

build:
	clang -I$(OCAML_LIBS) -c hello.c
	ocamlc -custom hello.ml hello.o -o hello

run: build
	./hello

clean:
	rm -f hello hello.o hello.cmi hello.cmo
