let fix_int_lit_size = ref (None : int option)

let set_size n =
  fix_int_lit_size := Some(n)

let is_set () = !fix_int_lit_size <> None

let get_size_type () =
  match !fix_int_lit_size with
  | None -> assert false
  | Some z -> z
