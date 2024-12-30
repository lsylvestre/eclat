(* runtime library for code generated from Eclat Compiler *)

let id_ x = x

module Bool = struct
  let land_ (a,b) = a && b
  let lor_ (a,b) = a || b
  let lxor_ (a,b) = if a then not b else b
  let lnot_ = not
  let if_ (a,b,c) = if a then b else c
end

module Int = struct
  (* assume integer operations in the source code 
   (with integer sizes less than 64) never cause overflow *)
  type t = Int64.t
  let neg_ a = Int64.neg a
  let absv_ a = Int64.abs a
  let add_ (a,b) = Int64.add a b
  let sub_ (a,b) = Int64.sub a b
  let mul_ (a,b) = Int64.mul a b
  let div_ (a,b) = Int64.div a b
  let modulo_ (a,b) = Int64.rem a b
  let lor_ (a,b) = Int64.logor a b
  let land_ (a,b) = Int64.logand a b
  let lxor_ (a,b) = Int64.logxor a b
  let lt_ (a,b) = Int64.compare a b < 0
  let le_ (a,b) = Int64.compare a b <= 0
  let ge_ (a,b) = Int64.compare a b >= 0
  let gt_ (a,b) = Int64.compare a b > 0
  let eq_ (a,b) = Int64.compare a b = 0
  let neq_ (a,b) = Int64.compare a b <> 0
  let lsr_ (a,b) = Int64.shift_right_logical a (Int64.to_int b)
  let asr_ (a,b) = Int64.shift_right a (Int64.to_int b)
  let lsl_ (a,b) = Int64.shift_left a (Int64.to_int b)
  let resize_ (v,n) = Int64.rem v (Int64.shift_left 1L n)
  let get_bit_(v,i) =
   if Int64.compare (Int64.logand (Int64.shift_right v (Int64.to_int i)) 1L) 1L = 0 
   then true 
   else false

  let to_int_ = Int64.to_int
  let of_int_ = Int64.of_int
end
let eclat_getBit = Int.get_bit_


module Print = struct
  let int_ n = print_int (Int64.to_int n)
  let string_ = print_string
  let newline_ = print_newline
end

module Vector = struct
  type 'a t = 'a array
  let make_ (n,v0) = Array.make n v0
  let length_ x = Array.length x
  let nth_ (x,n) = Array.get x (Int.to_int_ n)
  let copy_with_ (x,n,v) = 
    let a = Array.copy x in
    a.(n) <- v;
    a
  let infos_ (v : 'a) = 
    let size = Array.length v in
    assert (size > 0);
    let witness = Array.get v 0 in
    (Int.of_int_ size,witness)
end
module Vect = Vector

module Lock : sig
  type t
  val init : unit -> t
  val is_taken : t -> bool
  val acquire : t -> unit
  val release : t -> unit
end = struct
  type t = bool ref
  let init () = ref false
  let is_taken lock = !lock 
  let acquire lock = assert (not !lock); lock := true
  let release lock = assert (!lock); lock := false
end

module Bitvector : sig
  (** the Eclat compiler use {basic value - bitvector} conversions 
     for compiling sum types. Welltypeness (and safety) 
     is the responsability of the compiler 
  *) 
  type t
  val encode : 'a -> t
  val decode : t -> 'a
  val dummy : t
end =
struct
  type t = string 
  let encode v = (Marshal.to_string v [No_sharing])
  let decode s = Marshal.from_string s 0
  let dummy = String.empty
end
