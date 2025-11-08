(* CODE GENERATED FOR CREATING data_rom.mif INITIALIZATION FILE *)

let init_data () =
let global_start = 0 in
  exec
  print_string "WIDTH="; print_int (size_of_val (val_unit)); print_string ";"; print_newline ();
  print_string "DEPTH=2048;"; print_newline ();
print_string "ADDRESS_RADIX=DEC;"; print_newline ();print_string "DATA_RADIX=BIN;"; print_newline ();print_string "CONTENT BEGIN"; print_newline ();print_string "[0..11] : "; print val_unit; print_string ";"; print_newline ();
let x1 = global_start + 12 in
print_string "END;"; print_newline ();(let rec f () = f () in f ()) default ()
 ;;