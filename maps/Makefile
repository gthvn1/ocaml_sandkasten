.PHONY: test fmt

test:
	dune exec ./maps_test.exe

fmt:
	ocamlformat -i --enable-outside-detected-project maps.ml
	ocamlformat -i --enable-outside-detected-project maps_test.ml
