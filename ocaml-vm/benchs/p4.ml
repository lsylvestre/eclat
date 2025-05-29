(*****************************************************************************)
(*****  Devellopement d'applications avec Objective Caml                 *****)
(*****                                                                   *****)
(*****  Application : Jeux a deux joueurs : Puissance 4                  *****)
(*****************************************************************************)

module P4_rep = struct
  type cell = A | B | Vide 
  type jeu = cell array array 
  type coup = int 
  let col = 7 and lig = 6 
  let jeu_depart () = Array.create_matrix lig col Vide 

  let coups_legaux b m =   
    let l = ref [] in 
    for c = 0 to col-1 do if m.(lig-1).(c) = Vide then l := (c+1) :: !l done;
    !l

  let ajoute mat c =  
    let l = ref lig 
    in while !l > 0 & mat.(!l-1).(c-1) = Vide do  decr l done ; !l + 1

  let jouer_gen cp m e =
    let mj = Array.map Array.copy  m 
    in  mj.((ajoute mj cp)-1).(cp-1) <- e ; mj

  let jouer b cp m = if b then jouer_gen cp m A else jouer_gen cp m B
end ;;

module P4_eval = struct
  open P4_rep
  type jeu = P4_rep.jeu
  let valeur = Array.of_list [0; 2; 10; 50] 
  exception Quatre of int 
  exception Valeur_nulle 
  exception Arg_invalid 
  let moinsI = -10000
  let plusI = 10000

  let eval_quatre m l_dep c_dep delta_l delta_c =
    let n = ref 0 and e = ref Vide 
    and x = ref c_dep  and  y = ref l_dep 
    in try 
         for i = 1 to 4 do 
           if !y<0 or !y>=lig or !x<0 or !x>=col then raise Arg_invalid ; 
           ( match m.(!y).(!x) with 
                 A    -> if !e = B then raise Valeur_nulle ;
                         incr n ;
                         if !n = 4 then raise (Quatre plusI) ;
                         e := A
               | B    -> if !e = A then raise Valeur_nulle ;
                         incr n ;
                         if !n = 4 then raise (Quatre moinsI);
                         e := B; 
               | Vide -> ()                                      ) ;
           x := !x + delta_c ;
           y := !y + delta_l  
         done ; 
         valeur.(!n) * (if !e=A then 1 else -1)
       with 
           Valeur_nulle | Arg_invalid  -> 0

  let eval_bloc m e cmin cmax lmin lmax dx dy = 
    for c=cmin to cmax do for l=lmin to lmax do 
        e := !e + eval_quatre m l c dx dy
    done done

  let evaluer b m = 
  try let evaluation = ref 0 
      in (* evaluation des lignes *)
         eval_bloc m evaluation 0 (lig-1) 0 (col-4) 0 1 ;
         (* evaluation des colonnes *)
         eval_bloc m evaluation 0 (col-1) 0 (lig-4) 1 0 ;
        (* diagonales partant de la premiere ligne (à droite) *)
        eval_bloc m evaluation 0 (col-4) 0 (lig-4) 1 1 ;
        (* diagonales partant de la premiere colonne (à droite) *)
        eval_bloc m evaluation 1 (lig-4) 0 (col-4) 1 1 ;
        (* diagonales partant de la premiere ligne (à gauche) *)
        eval_bloc m evaluation 3 (col-1) 0 (lig-4) 1 (-1) ;
        (* diagonales partant de la derniere colonne (à gauche) *)
        eval_bloc m evaluation 1 (lig-4) 3 (col-1) 1 (-1) ;
        !evaluation
  with Quatre v -> v

  let est_feuille b m = let v = evaluer b m 
                        in v=plusI or v=moinsI or coups_legaux b m = []

  let est_stable b j = true

  type etat = G | P | N | C 

  let etat_de joueur m =
    let v = evaluer joueur m 
    in if v = plusI then if joueur then G else P
       else if v = moinsI then if joueur then P else G
       else if coups_legaux joueur m = [] then N else C
end ;;
