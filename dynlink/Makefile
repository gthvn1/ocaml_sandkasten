.PHONY: run build clean

run: build
	./hello

build:
	ocamlc -c hello.ml
	ocamlc -c hello_dyn.ml
	ocamlc -c main.ml
	ocamlc -I +dynlink dynlink.cma -o hello hello.cmo main.cmo

clean:
	rm -f *.cmi *.cmo *.cmx *.o hello
