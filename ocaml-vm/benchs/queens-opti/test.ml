(*
 $  make SRC="benchs/queens-opti/queens_v1.ml"  \
        FLAGS+="-ocaml"                        \
        BCOPT="-load-code -load-data" byte
 $ ocamlc runtime.ml  main_step.ml 
 $ ./a.out -run 100000

*)


let array_nth(a,i) =
  a.(i) ;;

let array_copy_with(a,i,v) =
  (* let a' = Array.copy a in *)
  a.(i) <- v; a
(*
let u = Array.make 10 1 ;;

let z = Array.copy u ;;

z.(5) <- 55;;
print_int 41;;*)




let abs_sub (a, b) = if b > a then b-a else a-b;;
let safe (p, q, d) = p <> q && abs_sub(q, p) <> d;;

let rec safe_all_aux (i, n, p,chess) =
  if i > n then true
  else ( if safe(array_nth(chess,i-1), p, n+1-i) 
         then (safe_all_aux(i+1, n, p,chess))
         else false ) ;;

let safe_all (n,p,chess) = safe_all_aux (1,n,p,chess) ;;

let rec loop (first_i,col, i, n,chess, nb) = 
  (* print_int i;
   print_int n; *)
 if i > n then 
   ( if col >= first_i then ( loop(first_i,col-1, array_nth(chess,(col-1))+1, n,chess, nb)) else nb)
 else (
   if safe_all (col,i,chess) then 
     ( let chess = array_copy_with(chess,col,i) in
       let nb2 = if col+1 = n then ( (* print_string "[found!]";*) (* show_chess(chess) ;*)  nb+1) else nb in
       loop(first_i,col+1, 1, n,chess,nb2) 
     )
   else 
     loop(first_i,col, i+1,n,chess, nb) )
   ;;


let queens_v1(n) =
   let v = Array.make n 1 in
   print_int (Array.length v);
   loop (1, 1, 1, Array.length v,v,0) ;;

let main () =
    queens_v1 12 ;;

main ();;

