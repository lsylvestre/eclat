(* ===============================================================
pour simuler le programme :
==================================================================
$ cd eclat_compiler 
$ ./eclat ../advent-of-code/extra.ecl ../advent-of-code/sum_input.ecl -arg="()" -ocaml
$ cd ../target
$ make EXTERNALS=../advent-of-code/extra.vhdl NS=50000
$ cd ml
$ /* ajouter "open Extra ;;" en tête de main_step.ml */
$ ocamlopt -I=../../advent-of-code runtime.ml ../../advent-of-code/extra.ml main_step.ml 
$ ./a.out -run 500
=============================================================== *)

let tab = create<50>() ;;

let load_tab by =
  for i = 0 to bytes_length by - 1 do
    set(tab,i,(bytes_get(by,i))) 
  done ;;

let sum_tab x =
  let r = create<1>() in
  set(r,0,0);
  for i = 0 to length x - 1 do
    let n = resize_int<32>(char_code (get(tab,i))) in
    set(r,0, (get(r,0) + n))
  done;
  get(r, 0) ;;

let chrono () = 
  reg (fun c -> c + 1) init (-1) ;;

let main () =
  let cy = chrono() in (* compteur de cycle *) 
  reg (fun ((),rdy) ->
    if rdy then
       (*** si c'est fini, 
           on affiche le temps d'exécution 
          et boucle sans rien faire 
        ***)
       let _ = exec print_string "temps d'exécution: ";
                    print_int cy;
                    print_newline ();
                    let rec fini() = fini() 
                    in fini()
               default ()
        in ((),true)
    else
      (*** si c'est pas fini, on continue à avancer pas à pas ***)
      exec
        print_string "lecture du fichier `input.txt`";
        print_newline ();
        let _ : char array<'N> = tab in
        let by = (input_file() : bytes<'N>) in
        bytes_print by;

        load_tab(by);
        let n = sum_tab(tab) in
        print_newline ();
        print_string "sommes des caractères: ";
        print_int n;
        print_newline ()
      default ()) init ((),false) ;;
