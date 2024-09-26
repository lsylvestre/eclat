
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
    ( if safe(vect_nth(chess,i-1), p, n+1-i) 
      then (safe_all_aux(i+1, n, p,chess))
      else false 
    ) ;;


 let safe_all (n,p,chess) = safe_all_aux (1,n,p,chess) ;;


(* la fonction de double récursion : (trouver une position pour la nouvelle reine, 
   quand on en a n, itérer le processus dans la configuration suivante : 

   col : col reines déjà traitées 
   i : en cours pour trouver une position correcte à la nouvelle reine (la col+1 ième)
   n : max total (invariable)
   v   : vecteur des reines déjà trouvées (en fait une pile une pile) 
   nb  : nombre de solutions déjà trouvées
*)
 

let rec loop (col, i, n,chess, nb) = 
 if i <= n then (
   if safe_all (col,i,chess) then 
     ( let chess = vect_copy_with(chess,col,i) in
       let nb2 = if col+1 = n then ( print_string "foo"; (* show_chess(chess) ;*)  nb+1) else nb in
       loop(col+1, 1, n,chess,nb2) 
     )
   else 
     loop(col, i+1,n,chess, nb) )
 else 
   ( if col > 0 then ( loop(col-1, vect_nth(chess,(col-1))+1, n,chess, nb)) else nb)
;;

let queens (v) =
  print_int (loop (1, 2, vect_size v,v,0)) ;;

let main () = 
  let v = vect_make (size_create 8,1) in
  queens v ;;


