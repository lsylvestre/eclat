let abs_sub (a, b) = if b > a then b-a else a-b;;

let safe (p, q, d) = p <> q && abs_sub(q, p) <> d;;

(** check that the n first queens in [stk] cannot attack the queen at position [p] *)
let safe_all (stk, n, p) = 
  let rec check i =
    if i > n then true else 
    if safe(stk_nth(stk, i-1), p, n+1-i) then check (i+1)
    else false
  in check 1 ;;

let rec loop (col, i, n, stk, acc) = 
 if i > n then (
    if col = 0 then acc else 
    loop(col-1, stk_nth(stk,col-1)+1, n, stk, acc)
 ) else (
   if safe_all (stk, col, i) then ( 
      let stk' = stk_update(stk,col,i) in
      let acc' = if col+1 = n then add_solution(stk', acc) else acc in
      loop(col+1, 1, n, stk', acc') ) 
   else loop(col, i+1,n, stk, acc)) ;;

let queens_with (n, stk, acc) = 
  loop (1, 1, n, stk, acc) ;;
