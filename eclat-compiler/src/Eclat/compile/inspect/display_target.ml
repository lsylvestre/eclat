
type print_mode = Any | Fsm | Flat

let string_of_print_mode s =
  match s with
  | "fsm" -> Fsm
  | "flat" -> Flat
  | _ -> failwith "unknown print mode"

let print_mode = ref Any

let set_print_mode s =
  let pm = string_of_print_mode s in
  print_mode := pm

let display a fsm =
  if a <> !print_mode then () else
  let open Format in
  fprintf std_formatter "@[<v>{debug mode}===========@,@.%a@]"
    MiniHDL_syntax.Debug.pp_fsm fsm
