SRC=../tests/v42.ml

OPT = -without-runtime -no-alias-deps   -no-app-funct \
	  -no-check-prims   -noassert    -noautolink    -nolabels

MORE_OPT=

BCOPT=

all:	tmp
	ocamlc $(OPT) -nostdlib -nopervasives -I ../stdlib ../stdlib/stdlib.ml
	ocamlc $(OPT) -a ../stdlib/stdlib.cmo  -I ../stdlib -o ../stdlib/stdlib.cma
	ocamlc  -nostdlib -nopervasives  -I ../stdlib ../stdlib/stdlib.cma -open Stdlib -w -31 \
                     ../stdlib/std_exit.ml \
                     ../stdlib/list.ml \
                     ../stdlib/array.ml \
                     ../stdlib/stack.ml \
                     ../stdlib/map.ml \
                     ../stdlib/char.ml \
                     ../stdlib/string.ml \
	                 ../stdlib/platform.ml \
	                 $(SRC) $(OPT) $(MORE_OPT)
	ocamlclean a.out
	echo $(BCOPT)
	dune exec -- bcprepare a.out $(BCOPT)

tmp:
	mkdir tmp

clean:
	rm -rf a.out
	rm -rf tmp/code.txt
	dune clean
