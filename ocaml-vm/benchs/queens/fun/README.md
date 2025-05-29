
```
$ make SRC="../benchs/queens/fun/config.ml \
               ../benchs/queens/fun/solve.ml \
               ../benchs/queens/fun/queens.ml" \
               MORE_OPT="-I=../benchs/queens/fun" \
               FLAGS="-ocaml" \ BCOPT="-load-code -load-data" \
               byte
   $ make vm
   $ cd ../target/ml
   $ ocamlc runtime.ml main_step.ml
   $ ./a.out -run 400000000
```


make SRC="../benchs/queens/fun/queens_ext.ml" \
       MORE_OPT="-I=../benchs/queens/fun" \
       FLAGS="-ocaml" \
       BCOPT="-load-code -load-data -custom caml_queens" \
       CUSTOM="ocaml-vm/benchs/queens/fun/config.ecl \
               ocaml-vm/benchs/queens/fun/solve.ecl  \
               ocaml-vm/benchs/queens/fun/caml_queens.ecl" \
       byte