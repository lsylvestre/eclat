
type print_mode = Any | Front | Let_floating | Lambda_lifting
                | Specialize | Inline | Specialize_ref | Propagation
                | Matching | Anf | MiddleEnd | GlobalizeArrays

let string_of_print_mode s =
  match s with
  | "none" -> Any
  | "front" -> Front
  | "float" -> Let_floating
  | "lift" -> Lambda_lifting
  | "spec" -> Specialize
  | "inl" -> Inline
  | "spec-ref" -> Specialize_ref
  | "prop" -> Propagation
  | "match" -> Matching
  | "anf" -> Anf
  | "middle-end" -> MiddleEnd
  | "glob-arrays" -> GlobalizeArrays
  | _ -> failwith "unknown print mode"

let print_mode = ref Any
let print_mode_str = ref ""

let set_print_mode s =
  let pm = string_of_print_mode s in
  print_mode := pm;
  print_mode_str := s

let display_pi a pi =
  if a <> !print_mode then () else
  let open Format in
  fprintf std_formatter "@[<v>======= INTERNAL REPRESENTATION (%s) =======@,%a@,=================================%s=========@,@]" 
     !print_mode_str Ast_pprint.pp_pi pi (String.make (String.length !print_mode_str) '=')

