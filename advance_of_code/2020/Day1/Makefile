INPUTS=input.txt

.PHONY: test

test: main.native main.byte $(INPUTS)
	./main.native $(INPUTS)

main.native: main.ml
	ocamlopt -o $@ $<

main.byte: main.ml
	ocamlc -o $@ $<

.PHONY: clean
clean:
	rm -f *.o *.cmi *.cmo *.cmx main.native main.byte
