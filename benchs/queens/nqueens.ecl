
(* sequential version *)
let reduce(f,v0,a) =
  let rec aux(acc,i) = 
    if i < (length a) then
      let x = get(a,i) in 
      aux(f(x,acc),i+1)
    else acc
  in aux(v0,0) ;;


let map_worker (f,r,src,dst) =
  let rec loop() =
    let i = get(r,0) in
    if i < length src then (
      let ((),v) = (set(r,0,i+1) ||
                    get(src,i)) in
      set(dst,i,f(v)); 
      loop())
    else ()
  in loop();;

let map_farm((n,f,src),k) =
  let _ : 'B array<'N> = src in
  let dst = create<'N>() in
  let r = array_create<1>() in
  set(r,0,0);
  parfor i = 1 to n do 
    map_worker(f,r,src,dst)
  done; 
  k(dst) ;;


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


let sz = 8 ;;

(* optimization *)
let safe_all (n,p,chess) =
  macro_generate (fun (i,acc) -> 
    acc & ((i+1 > n) or safe(vect_nth(chess,resize_int<16>(i)), p, n-i))
  ) true sz ;;


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
     ( let chess = vect_copy_with(chess,resize_int<32>(col),i) in
       let nb2 = if col+1 = n then ( print_string "[find]"; (* show_chess(chess) ;*)  nb+1) else nb in
       loop(restart,col+1, 1, n,chess,nb2) 
     )
   else 
     loop(restart,col, i+1,n,chess, nb) )
 else 
   ( if col >= restart then ( loop(restart,col-1, vect_nth(chess,(resize_int<16>(col-1)))+1, n,chess, nb)) else nb)
;;

let nqueens_sub_problem(i) =
  let v = vect_create<8> 1 in
  let v = vect_copy_with(v,0,i) in
  let n = vect_size v in 
  let n = loop (2, 1, 1, n,v,0) in
  print_string "rang "; print_int i; print_string ": "; print_int n; print_newline ();
  n

  ;;




let counter () : int<128> =
  let o = reg (fun c -> c + 1) init (-1) in o ;;
  
let nqueens ((b1, b2,_) : bool * bool*int<10>) : (bool * int<57>) =
  if b1 or b2 then  (true,0) else (
    let cy = counter () in
    let (result,rdy) = exec
        let src = create<8>() in
        for i = 0 to length src - 1 do set(src,i,i+1) done;
        map_farm(4,nqueens_sub_problem,src) @@ fun dst ->
        reduce((+),0,dst)
      default 0 in
    (if rdy then (print_string "cy:"; print_int cy;
      print_newline (); print_newline ();
      print_string "|number of solutions:"; print_int result; 
      print_newline (); print_newline ()) else ()); 
    let led = reg (fun last_val -> last_val or rdy) init false in
    (led,0)) ;;

let main = nqueens;;
