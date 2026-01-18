(* $ make SRC="benchs/queens/queens3.ml"  FLAGS="-ocaml" BCOPT="-load-code -load-data" byte
   $ make SRC="benchs/queens/queens3.ml"  FLAGS="-ocaml" BCOPT="-load-code -load-data" vm
   $ cd ../target/ml
   $ ocamlc runtime.ml main_step.ml
   $ ./a.out -run 400000000


   (*  CY=20207779 NBI=4997105 *)
 *)


let collect = ref [] ;;

let abs_sub (a, b) = if b > a then b-a else a-b;;

let safe (p, q, d) = p <> q && abs_sub(q, p) <> d;;

let rec safe_all_aux (qs, i, n, p) =
  if i > n then true
  else 
    ( if safe(qs.(i-1), p, n+1-i) 
      then (safe_all_aux(qs, i+1, n, p))
      else false 
    ) ;;

let safe_all (q,n,p) = safe_all_aux (q,1,n,p) ;;

let rec loop (col, i, n, v, nb) = 
 if i <= n then (
   if safe_all (v,col,i) then 
     ( v.(col) <- i ;
       let nb2 = if col+1 = n then (collect := Array.copy(v) :: !collect; nb+1) else nb in
       loop(col+1, 1, n, v, nb2)  
     )
   else 
     loop(col, i+1,n, v, nb) )
  else 
   ( if col >= 1
     then ( loop(col-1, v.(col-1)+1, n, v, nb)) else nb)
  ;;

let queens n = 
  collect := [];
  let chess = Array.create n 1 in
  loop (1, 1, n, chess, 0);
  print_int (List.length !collect);;

queens(10);;

(* for i = 0 to 999 do
  queens(9)
done
*)
;;