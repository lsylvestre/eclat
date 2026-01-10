open Extra;;
open Runtime

type state_t = IDLE91

let state = ref IDLE91 ;;
let _619 : Int.t ref = ref 0L ;;
let _615 : 'vv10 ref = ref (Array.make 3 ((default_char ()))) ;;
let _614_dir : 'vv11 ref = ref (default_char ()) ;;
let _613_by : 'vv12 ref = ref (default_bytes ()) ;;
let _622 : 'vv13 ref = ref (Array.make 2 ((default_char ()))) ;;
let _620 : Int.t ref = ref 0L ;;
let _616_pos : 'vv14 ref = ref (default_bytes ()) ;;
let rdy90 : bool ref = ref false ;;let _617 : unit ref = ref () ;;
let _618 : unit ref = ref () ;;let _621 : unit ref = ref () ;;
let result89 : unit ref = ref () ;;
let main_step =
  fun arg ->
    let argument = ref arg in
    (match !state with
    | IDLE91 -> 
      rdy90 := false;
      _613_by := IOFile.read_file_ (256, 24)("../advent-of-code/rot/instr_hexa.txt");
      _614_dir := Bytes.get_ (56, 8)((!_613_by, 0L));
      _615 := Bytes.to_vect_ (!_613_by);
      _622 := Vect.tail_ (24, 16)(!_615);
      _616_pos := Bytes.from_vect_ (!_622);
      _617 := Char.print_ (!_614_dir);
      _618 := Print.string_ (":");
      _619 := Bytes.to_hex_ (!_616_pos);
      _620 := Int.resize_(!_619,32);
      _621 := Print.int_ (!_620);
      result89 := Print.newline_ (());
      rdy90 := true;
      state := IDLE91
        
    );
  !result89
  

let run n =
  let args_list = [|()|] in
  let k = Array.length args_list - 1 in
  for i = 0 to n - 1 do
    ignore (main_step (args_list.(min i k)))
  done;;
  

let () =
  Arg.parse [
    ("-run", Arg.Int (fun n -> run n), "execute the given number of cycles")
  ]
  (fun _ -> ()) "Usage:
  ./eclat file"
;;

