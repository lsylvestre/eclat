
let bnot n = int_of_bool (n == 0) ;;

let addint (n,m) = n + m ;;
let subint (n,m) = n - m ;;
let mulint (n,m) = n * m ;;

let divint (n,m) =
  if m == 0 then 0 (* todo: raise error *) else
  let rec div (a,b,acc) =
    if a < b then acc else div(a-b, b, acc+1)
  in
  let r = div(abs n, abs m, 0) in
  if (n >= 0) xor (m >= 0) then 0 - r else r ;;

let modint (n,m) =
  if m == 0 then 0 (* todo: raise error *) else
  let rec modulo (a,b) =
    if a < b then a else modulo(a-b, b)
  in
  let r = modulo(abs n, abs m) in
  if (n < 0) then 0 - r else r ;;

let andint (n,m) = n land m ;;
let orint (n,m) = n lor m ;;
let xorint (n,m) = n lxor m ;;
let lslint (n,m) = n lsl m ;;
let lsrint (n,m) = n lsr m ;;
let asrint (n,m) = n asr m ;;

let eq (n,m) = int_of_bool (n == m) ;;
let neq (n,m) = int_of_bool (n <> m) ;;
let ltint (n,m) = int_of_bool (n < m) ;;
let leint (n,m) = int_of_bool (n <= m) ;;
let gtint (n,m) = int_of_bool (n > m) ;;
let geint (n,m) = int_of_bool (n >= m) ;;

let compare_imm(n1,n2) =
  if n1 < n2 then -1 else
  if n1 > n2 then 1 else 0 ;;


(* stack manipulation *)


let pop_stack_implace (sp_minus_1) =
  (* assert (sp[0] > 0); (see bug 3) *)
  let v = ram[sp_minus_1] in
  v ;;

(*** unsigned comparison (<)                  ***)
(*** n1 < 0 && n2 >= 0 => (ultint n1 n2) ~> 0 ***)
let ultint (n1,n2) =
  if n1 < 0 then (if n2 < 0 then gtint(n1,n2) else 0)
  else (if n2 < 0 then 0 else ltint(n1,n2)) ;;


(*** unsigned comparison (>=)                 ***)
(*** n1 < 0 && n2 >= 0 => (ugeint n1 n2) ~> 1 ***)
let ugeint (n1,n2) =
  if n1 < 0 then (if n2 < 0 then leint(n1,n2) else 1)
  else (if n2 < 0 then 1 else geint(n1,n2)) ;;


