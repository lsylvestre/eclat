(* $ make SRC="benchs/queens-opti/queens_v2.ml"  FLAGS+="-ocaml" \
          BCOPT="-load-code -load-data -custom caml_queens" \ 
          CUSTOM=ocaml-vm/benchs/queens-opti/queens_v2_ext.ecl vm


 *)

let chess_global = create<8>() ;;

let abs_sub (a, b) = if b > a then b-a else a-b;;
let safe (p, q, d) = p <> q && abs_sub(q, p) <> d;;

let rec safe_all_aux (i, n, p,chess) =
  if i > n then true
  else ( if safe(get(chess,i-1), p, n+1-i) 
         then (safe_all_aux(i+1, n, p,chess))
         else false ) ;;

let safe_all (n,p,chess) = safe_all_aux (1,n,p,chess) ;;

let rec loop (restart,col, i, n, nb) = 
 if i <= n then (
   if safe_all (col,i,chess_global) then 
     ( set(chess_global,col,i);
       let nb2 = if col+1 = n then (   (* print_string "[found!]";*) (* show_chess(chess) ;*)  
                   nb+1) 
                 else nb in
       loop(restart,col+1, 1, n,nb2) 
     )
   else 
     loop(restart,col, i+1,n, nb) )
 else 
   ( if col >= restart then ( loop(restart,col-1, get(chess_global,(col-1))+1, n, nb)) else nb)
;;


let caml_queens((v,_),st) =
   let i = long_val v in
   for k = 0 to i - 1 do set(chess_global,k,1) done;
   let n = loop (1, 1, 1, i,0) in
   (val_long n,st) ;;
