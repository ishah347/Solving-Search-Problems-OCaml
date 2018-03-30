all: collections gamesolve experiments tests

collections: collections.ml
	ocamlbuild collections.byte	

gamesolve: gamesolve.ml
	ocamlbuild gamesolve.byte	

experiments: experiments.ml
	ocamlbuild experiments.byte		

tests: tests.ml
	ocamlbuild tests.byte		

clean:
	rm -rf _build *.byte