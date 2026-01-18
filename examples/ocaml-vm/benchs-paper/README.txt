$ make SRC="../benchs-paper/queens.ml"  FLAGS+="-ocaml" BCOPT="-load-code -load-data" byte          CUSTOM=ocaml-vm/benchs/queens/queens4_ext.ecl 
$ make vm FLAGS+="-ocaml"
$ cd ../target/ml/
$ ocamlopt runtime.ml main_step.ml
$ ./a.out -run 1000000