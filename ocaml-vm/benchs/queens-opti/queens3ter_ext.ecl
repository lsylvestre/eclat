(* make SRC="benchs/queens/queens4.ml"  FLAGS+="-ocaml" BCOPT="-load_code -load_data" CUSTOM=ocaml-vm/benchs/queens/queens4_ext.ecl 

 *)
(* valeur absolue d'une soustraction *)
let abs_sub (a, b) = if b > a then b-a else a-b;;

(* two queens p and q are safe if : 
      - not in the same column
      - their vertical distance is not equal to their horizontal distance 
*)

let safe ((p, q, d) : int<5> * int<5> * int<5>) =
  p <> q && abs_sub(q, p) <> d;;


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

let sz = 12 ;;

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

let rec loop (restart,col, i, n,chess, nb) = print_string "!!!-";
 if i <= n then (
   if safe_all (col,i,chess) then 
     ( let chess = vect_copy_with(chess,col,i) in
       let nb2 = if col+1 = n then ( 
           print_string "[found!]"; 
           (* show_chess(chess) ;*)
           nb+1) 
           else nb in
       loop(restart,col+1, 1, n,chess,nb2) 
     )
   else 
     loop(restart,col, i+1,n,chess, nb) )
 else 
   (if col >= restart then 
      loop(restart,col-1, vect_nth(chess,(resize_int<32>(col-1)))+1, n,chess, nb)
    else nb)
;;


let queens_seq((n,chess):int<5> * int<5> vect<'a>) : short =
   let r = create<1>() in
   set(r,0,0);
   parfor i = 1 to sz do
     let chess_i = vect_copy_with(chess,0,i) in
     if i <= n then let res = loop (2, 1, 1,n,chess_i,0) in set(r,0,get(r,0)+res) else ()
   done;
   get(r,0)
   ;;

let caml_queens ((v,_),st) = print_string "hello!!!";
  let nb = let chess = vect_create<12> (1:int<5>) in
           (queens_seq(resize_int<5>(long_val(v)),chess))
  in
  print_int nb;
  (val_long(as_long(nb)), st)  ;;

