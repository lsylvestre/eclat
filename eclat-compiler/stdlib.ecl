(* **************** *)

(* bool *)

operator Bool.print : bool => unit @impure ;;

operator Bool.lnot : bool => bool ;;
operator Bool.lor :  (bool * bool) => bool ;;
operator Bool.land : (bool * bool) => bool ;;
operator Bool.lxor : (bool * bool) => bool ;;

(* **************** *)

(* int<'N> *)

operator Int.print :  int<'N> => unit  @impure ;;

operator Int.absv :    int<'N> => int<'N> ;;


operator Int.add :    (int<'N> * int<'N>) => int<'N> ;;
operator Int.sub :    (int<'N> * int<'N>) => int<'N> ;;
operator Int.neg :    int<'N> => int<'N> ;;
operator Int.mul :    (int<'N> * int<'N>) => int<'N>  ;;
operator Int.div :    (int<'N> * int<'N>) => int<'N> ;;
operator Int.modulo : (int<'N> * int<'N>) => int<'N> ;;
operator Int.eq :     (int<'N> * int<'N>) => bool  ;;
operator Int.neq :    (int<'N> * int<'N>) => bool  ;; 
operator Int.lt :     (int<'N> * int<'N>) => bool ;;
operator Int.le :     (int<'N> * int<'N>) => bool ;;
operator Int.gt :     (int<'N> * int<'N>) => bool  ;;
operator Int.ge :     (int<'N> * int<'N>) => bool ;;
operator Int.land :   (int<'N> * int<'N>) => int<'N> ;;
operator Int.lor :    (int<'N> * int<'N>) => int<'N> ;;
operator Int.lnot :   int<'N> => int<'N> ;;
operator Int.lxor :   (int<'N> * int<'N>) => int<'N> ;;
operator Int.land :   (int<'N> * int<'N>) => int<'N> ;;
operator Int.lsl :    (int<'N> * int<'OFFSET>) => int<'N> ;;
operator Int.lsr :    (int<'N> * int<'OFFSET>) => int<'N> ;;
operator Int.asr :    (int<'N> * int<'OFFSET>) => int<'N> ;;

let abs x = 
  if x < 0 then - x else x ;;

let max(a,b) =
  if a > b then a else b ;;

let min(a,b) =
  if a > b then b else a ;;

(* **************** *)

type uint<'N> ;;

operator Uint.print :  uint<'N> => unit @impure ;;

operator Uint.of_int : int<'N> => uint<'N> ;;
operator Uint.to_int : uint<'N> => int<'N> ;;

operator Uint.add :    (uint<'N> * uint<'N>) => uint<'N> ;;
operator Uint.sub :    (uint<'N> * uint<'N>) => uint<'N> ;;
operator Uint.mul :    (uint<'N> * uint<'N>) => uint<'N> ;;
operator Uint.div :    (uint<'N> * uint<'N>) => uint<'N> ;;
operator Uint.modulo : (uint<'N> * uint<'N>) => uint<'N> ;;
operator Uint.eq :     (uint<'N> * uint<'N>) => bool ;;
operator Uint.neq :    (uint<'N> * uint<'N>) => bool ;;
operator Uint.lt :     (uint<'N> * uint<'N>) => bool ;;
operator Uint.le :     (uint<'N> * uint<'N>) => bool  ;;
operator Uint.gt :     (uint<'N> * uint<'N>) => bool ;;
operator Uint.ge :     (uint<'N> * uint<'N>) => bool ;;

type `a vect<'b> ;;

operator%with_sizes Vect.create : `a => `a vect<'size> ;;

operator%with_sizes Vect.nth : (`a vect<'size> * int<16>) => `a;;
operator%with_sizes Vect.copy_with : (`a vect<'size> * int<16> * `a) => `a vect<'size>;;

operator%with_sizes Vect.infos : `a vect<'size> => int<16> * `a;;

let vect_nth (v,n) = Vect.nth(v,resize_int<16>(n)) ;;

let vect_copy_with(v,n,x) = Vect.copy_with(v,resize_int<16>(n),x) ;;
let vect_size v = 
  let (n,_) = Vect.infos v in
  resize_int<'a>(n) ;;


(*
type `a matrix<'d1,'d2> ;;

operator%with_sizes Matrix.create : `a => `a matrix<'d1,'d2> ;;
operator%with_sizes Matrix.get_line : (`a matrix<'d1,'d2> * int<16>) => `a vector<'d2> ;;
operator%with_sizes Matrix.copy_with_line : 
  (`a matrix<'d1,'d2> * int<16> * `a vector<'size>) => `a vector<'d2> ;;
operator%with_sizes Matrix.infos : `a matrix<'d1,'d2> => int<16> * `a;;


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


type fixed_point<'sf,'exp>@only_size_sum ;;

operator%with_sizes FixedPoint.create : int<'sf> => fixed_point<'sf,'exp> ;;

operator%with_sizes FixedPoint.significand : fixed_point<'sf,'exp> => uint<'sf> ;;
operator%with_sizes FixedPoint.exponent : fixed_point<'sf,'exp> => uint<'exp> ;;

operator FixedPoint.add : (fixed_point<'sf,'exp> * fixed_point<'sf,'exp>) => fixed_point<'sf,'exp> ;;
operator FixedPoint.sub : (fixed_point<'sf,'exp> * fixed_point<'sf,'exp>) => fixed_point<'sf,'exp> ;;

let print_fixed_point (f) =
  let x = FixedPoint.significand f in
  let y = FixedPoint.exponent f in
  Uint.print x; print_string " * 2^{"; 
  Uint.print y; print_string "}" ;; 
*)

