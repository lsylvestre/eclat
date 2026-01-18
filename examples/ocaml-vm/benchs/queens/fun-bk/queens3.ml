(* $ make SRC="benchs/queens/fun/queens3.ml"  FLAGS="-ocaml" BCOPT="-load-code -load-data" byte
   $ make SRC="benchs/queens/fun/queens3.ml"  FLAGS="-ocaml" BCOPT="-load-code -load-data" vm
   $ cd ../target/ml
   $ ocamlc runtime.ml main_step.ml
   $ ./a.out -run 400000000


   (*  CY=20207779 NBI=4997105 *)
 *)

(* ********************************** *)

type chessboard = int array ;;

let stack_update(cb,i,q) =
  let cb' = Array.copy cb in
  cb'.(i) <- q;
  cb' ;;

let stack_nth(cb,i) = cb.(i) ;;
let add_solution(stack,acc) = stack  :: acc ;;

(* ********************************** *)

let abs_sub (a, b) = if b > a then b-a else a-b;;

let safe (p, q, d) = p <> q && abs_sub(q, p) <> d;;

(** check that the n first queens in [stk] cannot attack the queen at position [p] *)
let safe_all (stk,n,p) = 
  let rec safe_all_aux (i) =
    if i > n then true else 
    if safe(stack_nth(stk,i-1), p, n+1-i) then 
      safe_all_aux(i+1)
    else false
  in safe_all_aux (1) ;;

let rec loop (col, i, n, stk, acc) = 
 if i > n then (
    if col = 0 then acc else 
    loop(col-1, stack_nth(stk,col-1)+1, n, stk, acc)
 ) else (
   if safe_all (stk,col,i) then ( 
      let stk' = stack_update(stk,col,i) in
      let acc' = if col+1 = n then add_solution(stk',acc) else acc in
      loop(col+1, 1, n, stk', acc') ) 
   else loop(col, i+1,n, stk, acc)) ;;

let queens_with (n, stk) = 
  loop (1, 1, n, stack, []) ;;
(*
let rec loop (col, i, n, stack, acc) = 
 if i <= n then (
   if safe_all (stack,col,i) then ( 
      let new_stack = stack_update(stack,col,i) in
      let new_acc = if col+1 = n
                then add_solution(new_stack,acc) 
                else acc
      in loop(col+1, 1, n, new_stack, new_acc)
   ) else loop(col, i+1, n, stack, acc)
 ) else (
    let q = stack_nth(stack,col-1) + 1 in
    if col >= 1 then ( loop(col-1, q, n, stack, acc)) else acc
  ) ;;*)

let queens n = 
  let stack = Array.create n 1 in
  queens_with (n, stk) ;;

print_int (queens(6));;

(* for i = 0 to 999 do
  queens(9)
done
*)
;;