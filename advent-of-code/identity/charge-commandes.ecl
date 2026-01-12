(* ************************************************** *
$ cd eclat_compiler 
$ ./eclat ../advent-of-code/extra.ecl ../advent-of-code/identity/charge-commandes.ecl -arg="()"
$ cd ../target
$ make EXTERNALS=../advent-of-code/extra.vhdl NS=100000
* ************************************************** *)

type com = L of int<32> | R of int<32> ;;

let commands : com array<4531> = create<4531>();;

let main () =
  exec
    array_from_file(commands,"../advent-of-code/identity/tab_init.txt");
    for i = 0 to length(commands) - 1 do
      let c = get(commands,i) in
      match c with
      | L(n) -> print_string "L"; print_int n  (* NB: affichage en décimal *)
      | R(n) -> print_string "R"; print_int n
    done;
    let rec finish() = finish() 
    in finish()
  default ();;
