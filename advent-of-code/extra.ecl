(****************************************************************************)
(** extension of the Eclat standard library (../eclat-compiler/stdlib.ecl) **)
(****************************************************************************)

(* all the operators below are synthesizable except:
   - Char.print 
   - Bytes.print
   - IOFile.read_file
   - IOFile.write_file *)

(**************************************************)
(**************** char and bytes ******************)
(**************************************************)

type char@8 ;;       (* new type constructor for 8-bits values *)
type bytes<'s>@8 ;;  (* sequence of chars *)

operator Char.code : char => int<8> ;;
operator Char.chr : int<8> => char ;;
operator Char.print : char => unit @impure ;; (* @impure denotes the side effect *)

(** the %with_sizes annotation means that the corresponding VHDL primitive
    requires two additional parameters : the size of the argument 
    and the size of the result **)

operator%with_sizes Bytes.make : char => bytes<'s> ;; 
operator            Bytes.len : bytes<'s> => int ;;
operator%with_sizes Bytes.get : (bytes<'s> * int) => char ;;
operator            Bytes.print : bytes<'s> => unit  @impure ;;
operator            Bytes.to_vect : bytes<'s> => char vect<'s> ;;
operator            Bytes.from_vect : char vect<'s> => bytes<'s> ;;

let char_code = Char.code ;;
let char_chr = Char.chr ;;
let char_print = Char.print ;;
let char_eq ((c1,c2) : char * char) = equal(c1,c2) ;; (* see stdlib.ecl *)

let bytes_make = Bytes.make ;;
let bytes_length = Bytes.len ;;
let bytes_get = Bytes.get ;;
let bytes_print = Bytes.print ;;
let bytes_to_vect = Bytes.to_vect ;;
let bytes_from_vect = Bytes.from_vect ;;

(**************************************************)
(************** file manipulations ****************)
(**************************************************)

operator%with_sizes IOFile.read_file : string => bytes<'s> @impure ;;
operator            IOFile.write_file : (string * bytes<'s>) => unit @impure ;;

let input_file = IOFile.read_file ;;
let output_file = IOFile.write_file ;;

(**************************************************)
(* generating .mif initialization file for arrays *)
(**************************************************)

let vect2mif v = 
  print_string "WIDTH="; print_int (size_val v); print_string ";"; print_newline ();
  print_string "DEPTH="; print_int (vect_size v); print_string ";"; print_newline ();
  print_string "ADDRESS_RADIX=DEC;"; print_newline (); print_string "DATA_RADIX=BIN;"; print_newline ();
  print_string "CONTENT BEGIN"; 
  for i = 0 to vect_size v - 1 do
    print_int i; print_string ":"; print (vect_nth (v,i)); print_string ";"; print_newline ()
  done;
  print_string "END;"; print_newline () ;;

let bytes2mif b =
  vect2mif (bytes_to_vect b) ;;
