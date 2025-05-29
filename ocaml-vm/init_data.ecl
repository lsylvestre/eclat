(* CODE GENERATED FOR CREATING data_rom.mif INITIALIZATION FILE *)

let init_data () =
let global_start = 0 in
  exec
  print_string "WIDTH=32;"; print_newline ();
  print_string "DEPTH=2048;"; print_newline ();
print_string "ADDRESS_RADIX=DEC;"; print_newline ();print_string "DATA_RADIX=BIN;"; print_newline ();print_string "CONTENT BEGIN"; print_newline ();print_string "[0..11] : "; print val_unit; print_string ";"; print_newline ();
let x1 = global_start + 24 in
(* ADD GLOBAL 12 *)
print_int (global_start + 12); print_string ":"; print (val_long(0)); print_string ";"; print_newline ();
(* ADD GLOBAL 13 *)
print_int (global_start + 13); print_string ":"; print (val_long(0)); print_string ";"; print_newline ();
(* ADD GLOBAL 14 *)
print_int (global_start + 14); print_string ":"; print (val_long(0)); print_string ";"; print_newline ();
(* ADD GLOBAL 15 *)
print_int (global_start + 15); print_string ":"; print (val_long(0)); print_string ";"; print_newline ();
(* ADD GLOBAL 16 *)
(* ========= *)
print_int x1; print_string ":"; print (val_long(make_header(0,2))); print_string ";"; print_newline ();
let x2 = x1+3 in
print_int (x1+1); print_string ":"; print (val_long(0)); print_string ";"; print_newline ();
print_int (x1+2); print_string ":"; print (val_long(0)); print_string ";"; print_newline ();
print_int (global_start + 16); print_string ":"; print (val_ptr(x1)); print_string ";"; print_newline ();
(* ADD GLOBAL 17 *)
print_int (global_start + 17); print_string ":"; print (val_long(0)); print_string ";"; print_newline ();
(* ADD GLOBAL 18 *)
print_int (global_start + 18); print_string ":"; print (val_long(0)); print_string ";"; print_newline ();
(* ADD GLOBAL 19 *)
print_int (global_start + 19); print_string ":"; print (val_long(0)); print_string ";"; print_newline ();
(* ADD GLOBAL 20 *)
print_int (global_start + 20); print_string ":"; print (val_long(0)); print_string ";"; print_newline ();
(* ADD GLOBAL 21 *)
print_int (global_start + 21); print_string ":"; print (val_long(0)); print_string ";"; print_newline ();
(* ADD GLOBAL 22 *)
print_int (global_start + 22); print_string ":"; print (val_long(0)); print_string ";"; print_newline ();
(* ADD GLOBAL 23 *)
print_int (global_start + 23); print_string ":"; print (val_long(0)); print_string ";"; print_newline ();
print_string "END;"; print_newline ();(let rec f () = f () in f ()) default ()
 ;;