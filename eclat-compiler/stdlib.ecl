(* type int<?n> ;;*)

operator Values.size : ~a => int<32> ;;
operator Values.equal : (~a * ~a) => bool ;;

let size_val = Values.size;;
let size_of_val = Values.size;;
let equal = Values.equal;;

(*******************************)
(************ bool *************)
(*******************************)

operator Bool.print : bool => unit @impure ;;

operator Bool.lnot : bool => bool ;;
operator Bool.lor :  (bool * bool) => bool ;;
operator Bool.land : (bool * bool) => bool ;;
operator Bool.lxor : (bool * bool) => bool ;;

operator Print.print_newline : unit => unit @impure ;;
operator Print.print_value   : ~a => unit @impure ;;
operator Print.print_string   : string => unit @impure ;;
operator Print.print_ascii   : ~a => unit @impure ;;

(*******************************)
(************* int *************)
(*******************************)

(* int<?n> *)

operator Int.print : int<?n> => unit  @impure ;;

operator Int.absv :    int<?n> => int<?n> ;;

operator%with_sizes Int.resize : int<?m> => int<?n> ;;
let int_resize<<?n>> (x) : int<?m> = Int.resize(x) ;;

operator Int.logical_not : int<?n> => int<?n> ;;
operator Int.get_bit : (int<?n> * int<32>) => bool ;;
operator Int.update_bit : (int<?n> * int<32> * bool) => int<?n> ;;
operator Int.add :    (int<?n> * int<?n>) => int<?n> ;;
operator Int.sub :    (int<?n> * int<?n>) => int<?n> ;;
operator Int.neg :    int<?n> => int<?n> ;;
operator Int.mul :    (int<?n> * int<?n>) => int<?n>  ;;
operator Int.div :    (int<?n> * int<?n>) => int<?n> ;;
operator Int.modulo : (int<?n> * int<?n>) => int<?n> ;;
operator Int.eq :     (int<?n> * int<?n>) => bool  ;;
operator Int.neq :    (int<?n> * int<?n>) => bool  ;; 
operator Int.lt :     (int<?n> * int<?n>) => bool ;;
operator Int.le :     (int<?n> * int<?n>) => bool ;;
operator Int.gt :     (int<?n> * int<?n>) => bool  ;;
operator Int.ge :     (int<?n> * int<?n>) => bool ;;
operator Int.land :   (int<?n> * int<?n>) => int<?n> ;;
operator Int.lor :    (int<?n> * int<?n>) => int<?n> ;;
operator Int.lnot :   int<?n> => int<?n> ;;
operator Int.lxor :   (int<?n> * int<?n>) => int<?n> ;;
operator Int.lsl :    (int<?n> * int<?ofs>) => int<?n> ;;
operator Int.lsr :    (int<?n> * int<?ofs>) => int<?n> ;;
operator Int.asr :    (int<?n> * int<?ofs>) => int<?n> ;;

let lnot = Int.logical_not ;;
let get_bit = Int.get_bit ;;
let update_bit = Int.update_bit ;;

let abs x = 
  if x < 0 then - x else x ;;

let max(a,b) =
  if a > b then a else b ;;

let min(a,b) =
  if a > b then b else a ;;

(********************************)
(************* uint *************)
(********************************)

type uint<?n> ;;

operator Uint.print :  uint<?n> => unit @impure ;;

operator Uint.of_int : int<?n> => uint<?n> ;;
operator Uint.to_int : uint<?n> => int<?n> ;;

operator Uint.add :    (uint<?n> * uint<?n>) => uint<?n> ;;
operator Uint.sub :    (uint<?n> * uint<?n>) => uint<?n> ;;
operator Uint.mul :    (uint<?n> * uint<?n>) => uint<?n> ;;
operator Uint.div :    (uint<?n> * uint<?n>) => uint<?n> ;;
operator Uint.modulo : (uint<?n> * uint<?n>) => uint<?n> ;;
operator Uint.eq :     (uint<?n> * uint<?n>) => bool ;;
operator Uint.neq :    (uint<?n> * uint<?n>) => bool ;;
operator Uint.lt :     (uint<?n> * uint<?n>) => bool ;;
operator Uint.le :     (uint<?n> * uint<?n>) => bool  ;;
operator Uint.gt :     (uint<?n> * uint<?n>) => bool ;;
operator Uint.ge :     (uint<?n> * uint<?n>) => bool ;;

(*******************************)
(************ vect *************)
(*******************************)

type ~a vect<?n> ;;

operator%with_sizes Vect.create : ~a => ~a vect<?n> ;;


let vect_make<<?n>> (x:`a) : `a vect<?n> = Vect.create(x) ;;

operator%with_sizes Vect.nth : (`a vect<?n> * int<32>) => `a;;
operator Vect.copy_with : (`a vect<?n> * int<32> * `a) => `a vect<?n>;;

operator%with_sizes Vect.infos : `a vect<?n> => int<32> * `a;;

operator Vect.cons : (`a * `a vect<?n>) => `a vect<?n+1>;;
operator%with_sizes Vect.head : (`a vect<?n+1>) => `a;;
operator%with_sizes Vect.tail : (`a vect<?n+1>) => `a vect<?n>;;
operator Vect.split : (`a vect<2*?n>) => (`a vect<?n>*`a vect<?n>);;
operator Vect.concat : (`a vect<?n>*`a vect<?n>) => `a vect<2*?n>;;
operator Vect.of_int : int<?n> => bool vect<?n>;;
operator Vect.to_int : bool vect<?n> => int<?n>;;

let vect_nth (v,n) = Vect.nth(v,n) ;;

let vect_copy_with(v,n,x) = Vect.copy_with(v,n,x) ;;
let vect_update = vect_copy_with ;;

let vect_size v = 
  let (n,_) = Vect.infos v in
  n ;;

let vect_cons (x,v) = Vect.cons(x,v) ;;
let vect_head v = Vect.head v ;;
let vect_tail v = Vect.tail v ;;
let vect_split v = Vect.split v ;;
let vect_concat v = Vect.concat v ;;

let vect_forall (f,v) =
  let _ : 'a vect<?n> = v in
  generate<1 to ?n>(fun (i,acc) -> acc & vect_nth(v,i)) init false ;; 

let vect_of_int = Vect.of_int;;
let int_of_vect = Vect.to_int;;

(**** ============================
    (* commented part: no matrix nor fixed_point
       for the moment at the moment *)

type `a matrix<?d1,?d2> ;;

operator%with_sizes Matrix.create : `a => `a matrix<?d1,?d2> ;;
operator%with_sizes Matrix.get_line : (`a matrix<?d1,?d2> * int<16>) => `a vector<?d2> ;;
operator%with_sizes Matrix.copy_with_line : 
  (`a matrix<?d1,?d2> * int<16> * `a vect<?d2>) => `a matrix<?d1,?d2> ;;
operator%with_sizes Matrix.infos : `a matrix<?d1,?d2> => int<32> * `a;;

let matrix_create = Matrix.create ;;
let matrix_get_line = Matrix.get_line ;;

let matrix_nth(m,i,j) = 
  let v = Matrix.get_line(m,i) in
  Vect.nth(v,j) ;;

let matrix_copy_with(m,i,j,x) = 
  let v = Matrix.get_line(m,i) in
  let v2 = Vect.copy_with(v,j,x) in
  Matrix.copy_with_line(m,i,v2);;

let matrix_size(m) = 
  let (n,_) = Matrix.infos(m) in
  let v = Matrix.get_line(m,0) in
  let p = vect_size(v) in 
  (p,n/p) ;;

let matrix_iter(f,m) =
  let (p,q) = matrix_size m in
  for i = 0 to p - 1 do
    for j = 0 to q - 1 do
      f(matrix_nth(m,i,j))
    done
  done ;;

type fixed_point<?sf,?exp>@only_size_sum ;;

operator%with_sizes FixedPoint.create : int<?sf> => fixed_point<?sf,'exp> ;;

operator%with_sizes FixedPoint.significand : fixed_point<?sf,'exp> => uint<'sf> ;;
operator%with_sizes FixedPoint.exponent : fixed_point<?sf,'exp> => uint<'exp> ;;

operator FixedPoint.add : (fixed_point<?sf,?exp> * fixed_point<?sf,?exp>) => fixed_point<?sf,?exp> ;;
operator FixedPoint.sub : (fixed_point<?sf,?exp> * fixed_point<?sf,?exp>) => fixed_point<?sf,?exp> ;;

let print_fixed_point (f) =
  let x = FixedPoint.significand f in
  let y = FixedPoint.exponent f in
  Uint.print x; print_string " * 2^{"; 
  Uint.print y; print_string "}" ;; 
=========================== *)


(********************************)
(* Esterel derivated constructs *)
(********************************)

operator%with_sizes Default.create : unit => 'a ;; (* unsafe *)

let halt() =
  loop: pause() end ;;

let sustain s = 
  loop: emit s; pause() end ;;

let await s = 
  trap T in 
  loop: pause(); if ?s then exit T else () end ;;

let abort (f,s) =
  trap T in [ (suspend f() when s; exit T) || (await s; exit T )] ;;

let loop_each (f,s) =
  loop: abort ((fun () -> (f(); halt())),s) end ;;

let await_immediate s =
  trap T in loop: if ?s then exit T; pause() end ;;

let suspend_when_immediate(f,s) =
  suspend (if ?s then pause(); f()) when s ;;

(*
let do_every(p,s) = 
  await s; loop: p() each s
*)

let abro (a,b,r,o) =
  loop_each((fun () -> 
    [await a || await b]; emit o),r) ;;

let abcro(a,b,c,r,o) =
  let t = signal<> in 
  [ abro(a,b,r,t) || abro(t,c,r,o) ] ;;

(*******************************)
(******* Lustre encoding *******)
(*******************************)

let absent () =
  let s = signal <> 
  in ?s ;;


let fby(x,y) =
  let s = signal<> in
  let pre_s = signal<> in
  emit s(y);
  let _ = exec 
            emit pre_s(x);
            loop:
              let z = ?s in pause(); 
              emit pre_s(z) 
            end default () in
  ?pre_s ;;

(**
let fby (x, y) =
  let shift (_, o) = (o, y) in
  let (o, _) = reg shift init (absent(), x)
  in o ;;
**)

let mux(a, b, c) = 
  if a then b else c ;;

let when_(f, clk) =
  if clk then f() else absent() ;;

let merge(clk, a, b) =
  if clk then a else b ;;


let fixpoint (f) =
  let s = signal <> in
  emit s(f(?s));
  ?s ;;


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
type bytes<?s>@8 ;;  (* sequence of chars *)

operator Char.code : char => int<8> ;;
operator Char.chr : int<8> => char ;;
operator Char.print : char => unit @impure ;; (* @impure denotes the side effect *)

(** the %with_sizes annotation means that the corresponding VHDL primitive
    requires two additional parameters : the size of the argument 
    and the size of the result **)

operator%with_sizes Bytes.make : char => bytes<?n> ;; 
operator            Bytes.len : bytes<?n> => int ;;
operator%with_sizes Bytes.get : (bytes<?n> * int) => char ;;
operator            Bytes.print : bytes<?n> => unit  @impure ;;
operator            Bytes.to_vect : bytes<?n> => char vect<?n> ;;
operator            Bytes.from_vect : char vect<?n> => bytes<?n> ;;
operator            Bytes.to_hex : bytes<?n> => int<2*2*?n> ;;

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
let bytes_to_hex = Bytes.to_hex ;;

let bytes_vect_map ((f,b):(char vect<?n> => char vect<?m>) * bytes<?n>) : bytes<?m> = 
  bytes_from_vect(f(bytes_to_vect b));;

let bytes_cons ((x,b):char * bytes<?n>) : bytes<?n+1> =
  let cons (v:char vect<?n>) : char vect<?n+1> = vect_cons(x,v) in  
  bytes_from_vect(cons(bytes_to_vect b));;

let bytes_tail (b:bytes<?n+1>) : bytes<?n> = 
  bytes_vect_map(vect_tail,b);;

let print_char = char_print ;;
let print_bytes = bytes_print ;;
(**************************************************)
(************** file manipulations ****************)
(**************************************************)

operator%with_sizes IOFile.read_file : string => bytes<?n> @impure ;;
operator            IOFile.write_file : (string * bytes<?n>) => unit @impure ;;

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

