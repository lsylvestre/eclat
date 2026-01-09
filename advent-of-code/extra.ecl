type char@8 ;;   (* nouveau type de valeurs sur 8 bit *)
type bytes<'s>@8 ;;

operator Char.code : char => int<8> ;;
operator Char.chr : int<8> => char ;;
operator Char.print : char => unit @impure ;;

operator%with_sizes Bytes.make : char => bytes<'s> ;;
operator Bytes.len : bytes<'s> => int ;;
operator%with_sizes Bytes.get : (bytes<'s> * int) => char ;;
operator Bytes.print : bytes<'s> => unit  @impure ;;

operator%with_sizes IOFile.read_file : string => bytes<'s> @impure ;;
operator IOFile.write_file : (string * bytes<'s>) => unit @impure ;;

let char_code = Char.code ;;
let char_chr = Char.chr ;;
let char_print = Char.print ;;

let bytes_make = Bytes.make ;;
let bytes_length = Bytes.len ;;
let bytes_get = Bytes.get ;;
let bytes_print = Bytes.print ;;

let input_file = IOFile.read_file ;;
let output_file = IOFile.write_file ;;

(* print in standard output .mif initialization file for array of n-bit values *)
let gen_mif (bytes,n) = 
  print_string "WIDTH="; print_int n; print_string ";"; print_newline ();
  print_string "DEPTH="; print_int ((bytes_length bytes / n) * 8); print_string ";"; print_newline ();
  print_string "ADDRESS_RADIX=DEC;"; print_newline (); print_string "DATA_RADIX=BIN;"; print_newline ();
  print_string "CONTENT BEGIN"; 
  for i = 0 to bytes_length bytes - 1 do
    print_int i; print_string ":"; print (bytes_get (bytes,i)); print_string ";"; print_newline ()
  done;
  print_string "END;"; print_newline () 
;;
