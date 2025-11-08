(* $ make SRC="benchs/queens-opti/queens_v2.ml"  FLAGS+="-ocaml" \
          BCOPT="-load-code -load-data -custom caml_queens" \ 
          CUSTOM=ocaml-vm/benchs/queens-opti/queens_v2_ext.ecl vm


 *)

let abs_sub (a, b) = if b > a then b-a else a-b;;
let safe (p, q, d) = p <> q && abs_sub(q, p) <> d;;

let rec safe_all_aux (i, n, p,chess) =
  if i > n then true
  else ( if safe(as_short(long_val(get_field(chess,i-1))), p, n+1-i) 
         then (safe_all_aux(i+1, n, p,chess))
         else false ) ;;

let safe_all (n,p,chess) = safe_all_aux (1,n,p,chess) ;;

let rec loop (restart,col, i, n, nb,chess) = 
 if i <= n then (
   if safe_all (col,i,chess) then 
     ( set_field(chess,col,val_long(as_long(i)));
       let nb2 = if col+1 = n then (   (* print_string "[found!]";*) (* show_chess(chess) ;*)  
                   nb+1) 
                 else nb in
       loop(restart,col+1, 1, n,nb2,chess) 
     )
   else 
     loop(restart,col, i+1,n, nb,chess) )
 else 
   ( if col >= restart then ( loop(restart,col-1, as_short(long_val(get_field(chess,(col-1))))+1, n, nb,chess)) else nb)
;;


let caml_queens((v,_),st) = (* print_string "~~~~~~>"; print_int 42;*)
   let i = as_short(long_val v) in
   print_string "foo:"; print_int i;
   let (pc,acc,sp, (env, extra_args, trap_sp), others) = st in
   let (acc,env,chess) = make_block(sp, acc, env, 0, i) in
   let st = (pc,acc,sp, (env, extra_args, trap_sp), others) in
   for k = 0 to i - 1 do imm_set_field(chess,k,val_long(1)) done;
   let n = loop (1, 1, 1, i,0,chess) in
   (val_long n,st) ;;

