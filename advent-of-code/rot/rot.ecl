(* ===============================================================
pour simuler le programme :
==================================================================
$ cd eclat_compiler 
$ ./eclat ../advent-of-code/extra.ecl ../advent-of-code/rot/rot.ecl -arg="()" -ocaml
$ cd ../target
$ make EXTERNALS=../advent-of-code/extra.vhdl NS=50000
$ cd ml
$ sed -i '' '1s/^/open Extra;;\n/' main_step.ml
$ ocamlopt -I=../../advent-of-code runtime.ml ../../advent-of-code/extra.ml main_step.ml
$ ./a.out -run 500
=============================================================== *)


let main () =
  let by = (input_file("../advent-of-code/rot/instr_hexa.txt") : bytes<3>) in
  (* exemple d'instruction : "LFA", c'est L suivi de l'entier 0xFA = 250 *)
  let dir = bytes_get(by,0) in
  let pos = bytes_tail(by) in
  char_print dir;
  print_string ":";
  print_int (resize_int<32>(bytes_to_hex(pos)));
  print_newline() ;;
