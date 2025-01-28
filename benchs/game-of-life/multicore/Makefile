FLAGS=-O3
N?=4

compile:
	ocamlfind ocamlopt -linkpkg -package domainslib $(FLAGS) v1.ml

run:
	./a.out $(N)

clean:
	rm -rf a.out *.cmi *.cmx *.o
