(* https://ris.utwente.nl/ws/portalfiles/portal/5438104/NQueensOnFPGA.pdf *)

(* on représente l'échiquier comme un vecteur d'une colonne 
    <4,7,5,3,1> représente en (abscisse, ordonnée) les reines :
    (1,4), (2,7), (3,5), (4,3), (5,1)  
*)

(* valeur absolue d'une soustraction *)
let abs_sub (a, b) = if b > a then b-a else a-b;;

(* deux reines p et q sont safe : 
      - pas dans la même colonne 
      - et la distance verticale n'est pas la même que la distance horizontale 
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


let sz = 9 ;;

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
 

let rec loop (restart,col, i, n,chess, nb) = 
 if i <= n then (
   if safe_all (col,i,chess) then 
     ( let chess = vect_copy_with(chess,col,i) in
       let nb2 = if col+1 = n then ( print_string "[found!]"; (* show_chess(chess) ;*)  nb+1) else nb in
       loop(restart,col+1, 1, n,chess,nb2) 
     )
   else 
     loop(restart,col, i+1,n,chess, nb) )
 else 
   ( if col >= restart then ( loop(restart,col-1, vect_nth(chess,(resize_int<32>(col-1)))+1, n,chess, nb)) else nb)
;;


let queens_seq(v:int<8> vect<'a>) =
   (loop (1, 1, 1,vect_size v,v,0)) ;;

let queens (v : int<8> vect<'a>) =
  let n = vect_size v in
  let r = create<1>() in
  set(r, 0,0);
  
  parfor i = 1 to sz do
    let v = vect_copy_with(v,0,i) in 
    let n = loop (2, 1, 1, n, v, 0) in
    print_string "rang "; print_int i; print_string ": "; print_int n; print_newline ();
    set(r,0,n+get(r,0))
  done;
  (get(r,0));;

let chrono (b) : int<64> =
  reg (fun c -> if b then c + 1 else c) init (-1) ;;


let main () = 
  let cy = chrono (true) in
  let (_, rdy) = exec
                    let v = vect_create<9> (1:int<8>) in
                    print_int (queens v)
                  default () 
  in
  if rdy then (print_newline (); 
               print_string "cy="; 
               print_int cy; 
               print_newline()) else () ;;


(*
let main (_ : int<12>) : outputs = 
  let (_, rdy) = exec
                    let v = vect_create<13> (1:int<8>) in
                    (queens v)
                  default 0
  in
  let x = reg (fun x -> rdy or x) init false in 
  let cy = chrono(not(x)) in 
  (all_leds(not(x)),number_to_alpha(resize_int<25>(cy/1024)));;
*)
