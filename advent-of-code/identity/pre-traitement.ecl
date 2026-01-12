(* ************************************************** *
$ cd eclat_compiler 
$ ./eclat ../advent-of-code/extra.ecl ../advent-of-code/identity/pre-traitement.ecl -arg="()"
$ cd ../target
$ make EXTERNALS=../advent-of-code/extra.vhdl NS=100000
$ ghdl -r  tb_main --vcd=tb.vcd > ../advent-of-code/identity/tab_init.txt
   ^C (après 2 secondes)
* ************************************************** *)


let commands : ((char * bytes<8>) array<4531>) = create<4531>();;

type com = L of int<32> | R of int<32> ;;

let main () =
  exec
    array_from_file(commands,"../advent-of-code/puzzle1-input0.txt");
    for i = 0 to length(commands) - 1 do
      let (c,b) = get(commands,i) in
      (* print_char c;
      print_bytes b;*)
      let n = bytes_to_hex b in
      let v = if c = char_chr 76 (* L *) then L(n) else R(n) in
      print_ascii v
    done;
    print_newline ();
    let rec finish() = finish() 
    in finish()
  default ();;
