(* make SRC="benchs/queens/sans-collect/queens4.ml"  FLAGS+="-ocaml" BCOPT="-load-code -load-data -custom caml_queens" CUSTOM=ocaml-vm/benchs/queens/sans-collect/queens4_seq_ext.ecl  byte

 *)
(* valeur absolue d'une soustraction *)
let abs_sub (a, b) = if b > a then b-a else a-b;;

(* two queens p and q are safe if : 
      - not in the same column
      - their vertical distance is not equal to their horizontal distance 
*)

let safe (p, q, d) = p <> q && abs_sub(q, p) <> d;;


(* une nouvelle reine p est safe vis-à-vis d'un ensemble de n reines q
 ssi  \forall i \ in[1..n] : S(q_i, p, n+1-i)
*)
 
let rec safe_all_aux (i, n, p,chess) =
  if i > n then true
  else 
    ( if safe(vect_nth(chess,(resize_int<32>(i-1))), p, n+1-i) 
      then (safe_all_aux(i+1, n, p,chess))
      else false 
    ) ;;

let safe_all (n,p,chess) = safe_all_aux (1,n,p,chess) ;;


let alloc_block(tag,size,st) =
  let (pc,acc,sp, (env, extra_args, trap_sp), others) = st in
  let (acc,env,blk) = pause_make_block(sp,acc,env,tag,size) in
  let st = (pc,acc,sp, (env, extra_args, trap_sp), others) in
  (blk,st) ;;

let sz = 10 ;;

let write_en = create<10>() ;;
let write_data = create<10>() ;;
let ref_st = create<1>() ;;
let ref_link = create<1>() ;;

(* (* optimization: *)
let safe_all (n,p,chess) =
  macro_generate (fun (i,acc) -> 
    acc & ((i+1 > n) or safe(vect_nth(chess,resize_int<32>(i)), p, n-i))
  ) true sz ;; 
*)

(* la fonction de double récursion : (trouver une position pour la nouvelle reine, 
   quand on en a n, itérer le processus dans la configuration suivante : 

   col : col reines déjà traitées 
   i : en cours pour trouver une position correcte à la nouvelle reine (la col+1 ième)
   n : max total (invariable)
   v   : vecteur des reines déjà trouvées (en fait une pile une pile) 
   nb  : nombre de solutions déjà trouvées
*)

let rec loop (id,restart,col, i, n,chess, nb) = print_string "!!!-";
 if i <= n then (
   if safe_all (col,i,chess) then 
     ( let chess = vect_copy_with(chess,col,i) in
       let nb2 = if col+1 = n then ( 
           print_string "[found!]"; 
           nb+1) 
           else nb in
       loop(id,restart,col+1, 1, n,chess,nb2) 
     )
   else 
     loop(id,restart,col, i+1,n,chess, nb) )
 else 
   (if col >= restart then 
      loop(id,restart,col-1, vect_nth(chess,(resize_int<32>(col-1)))+1, n,chess, nb)
    else nb)
;;


let queens_seq((n,v):int<8> * int<8> vect<'a>) =
   let n = loop (0,1, 1, 1,n,v,0) in
   (n,get(ref_st,0)) ;;

let caml_queens ((v,_),st) = print_string "hello!!!";
  let (link,st) = alloc_block(0,2,st) in
  let (nb,_) = let chess = vect_create<20> (1:int<8>) in
                 (queens_seq(resize_int<8>(long_val v),chess))
  in
  print_int nb;
  print_string "FIN-:";
  (link, st)  ;;


