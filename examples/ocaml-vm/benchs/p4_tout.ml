let read_line _ = "" ;;
let int_of_char _ = 0;;
(*****************************************************************************)
(*****  Devellopement d'applications avec Objective Caml                 *****)
(*****                                                                   *****)
(*****  Application : Jeux a deux joueurs : Alpha-Beta                   *****)
(*****************************************************************************)

module type REPRESENTATION = sig
  type jeu
  type coup
  val jeu_depart : unit -> jeu
  val coups_legaux: bool -> jeu -> coup list
  val jouer: bool -> coup -> jeu -> jeu
end

module type EVAL = 
sig
  type jeu
  val evaluer: bool -> jeu -> int
  val plusI : int
  val moinsI: int
  val est_feuille: bool -> jeu -> bool
  val est_stable: bool -> jeu -> bool
  type etat = G | P | N | C
  val etat_de : bool -> jeu -> etat
end

module type ALPHABETA = sig 
  type jeu
  type coup
  val alphabeta : int -> bool -> jeu -> coup
end ;;

module type FALPHABETA  = 
  functor (Rep : REPRESENTATION) ->
  functor (Eval : EVAL with type jeu = Rep.jeu) ->
    ALPHABETA with type jeu = Rep.jeu and type coup = Rep.coup ;;

(***************************************************************************)

module FAlphabetaO 
    (Rep : REPRESENTATION)  (Eval : EVAL with type jeu = Rep.jeu)  = 
  struct
    type jeu = Rep.jeu
    type coup = Rep.coup
    exception AlphaCoupure of int 
    exception BetaCoupure of int 
 
    let maxmin_iter noeud minmax_cur beta alpha cp =
      let alpha_resu =  
        max alpha (minmax_cur (Rep.jouer true cp noeud) beta alpha) 
      in if alpha_resu >= beta then  raise (BetaCoupure alpha_resu)  
         else alpha_resu 

    let minmax_iter noeud maxmin_cur alpha beta cp =
      let beta_resu = 
        min beta (maxmin_cur (Rep.jouer false cp noeud) alpha beta) 
      in if beta_resu <= alpha then  raise (AlphaCoupure beta_resu)  
         else beta_resu     

    let rec maxmin prof noeud alpha beta =
      if    (prof < 1 && Eval.est_stable true noeud) 
         || Eval.est_feuille true noeud
      then Eval.evaluer true noeud  
      else 
        try  let prev = maxmin_iter noeud (minmax (prof - 1)) beta
             in List.fold_left prev alpha (Rep.coups_legaux true noeud)
        with BetaCoupure a -> a

    and minmax prof noeud beta alpha =
      if    (prof < 1 && Eval.est_stable false noeud) 
         || Eval.est_feuille false noeud
      then Eval.evaluer false noeud 
      else 
        try  let prev = minmax_iter noeud (maxmin (prof - 1)) alpha
             in List.fold_left prev beta (Rep.coups_legaux false noeud)
        with AlphaCoupure b -> b

    let rec cherche a l1 l2 = match (l1,l2) with 
        (h1::q1, h2::q2) -> if a = h1 then h2  else cherche a q1 q2
      | ([], []) -> failwith  "" (* ("AB: "^(string_of_int a)^" non trouve'")*)
      | (_ ,  _) -> failwith  "AB: longueur diff" 

    (* val alphabeta : int -> bool -> Rep.jeu -> Rep.coup *)
    let alphabeta prof joueur  racine = 
      let alpha = ref Eval.moinsI and beta = ref Eval.plusI in
      let l = ref [] in
      let cpl = Rep.coups_legaux joueur  racine in
      let eval =
        try 
          for i = 0 to (List.length cpl) - 1 do
            if joueur then 
              let b = Rep.jouer joueur (List.nth cpl i) racine in
              let a = minmax (prof-1) b !beta !alpha 
              in l := a :: !l ; 
                 alpha := max !alpha a ;
                 (if !alpha >= !beta then raise (BetaCoupure !alpha) )
            else
              let a = Rep.jouer joueur (List.nth cpl i) racine in
              let b = maxmin (prof-1) a !alpha !beta 
              in l := b :: !l ;
                 beta := min !beta b ;
                 (if !beta <= !alpha then raise (AlphaCoupure !beta))
            done ;
          if joueur then !alpha else !beta
        with 
              BetaCoupure a -> a
           |  AlphaCoupure b -> b
      in    
         l := List.rev !l ;
         cherche eval !l cpl
  end ;;

module FAlphabeta = (FAlphabetaO : FALPHABETA);;

(***************************************************************************)

module type SQUELETTE = sig 
  val accueil: unit -> unit
  val init: unit -> ((unit -> unit) * (unit -> unit))
  val encore: unit -> bool
  val fin: unit -> unit
  exception Gagne
  exception Perd
  exception Nul
  val gagne: unit -> unit
  val perd: unit -> unit
  val nul: unit -> unit
end ;;

module FMain (P : SQUELETTE) = 
  struct
    let ca_joue tours = while true do (fst tours) () ; (snd tours) () done

    let main () = let fini = ref false 
                  in P.accueil ();
                     while not !fini do
                       ( try  ca_joue (P.init ())
                         with P.Gagne -> P.gagne ()
                            | P.Perd  -> P.perd ()
                            | P.Nul   -> P.nul ()    );
                       fini := not (P.encore ())
                     done ;
                     print_int 6666;
                     P.fin ()
  end ;;

module type AFFICHAGE = sig 
  type jeu 
  type coup
  val accueil: unit -> unit
  val fin: unit -> unit
  val gagne: unit -> unit
  val perd: unit -> unit
  val nul: unit -> unit
  val init: unit -> unit
  val affiche : bool -> coup -> jeu -> jeu -> unit
  val choix : bool ->  jeu -> coup
  val q_jouer : unit -> bool   
  val q_commencer : unit -> bool
  val q_continuer : unit -> bool
end ;;

module FSquelette 
    (Rep   : REPRESENTATION) 
    (Aff   : AFFICHAGE with type jeu = Rep.jeu and  type coup = Rep.coup) 
    (Eval  : EVAL with type jeu = Rep.jeu) 
    (Alpha : ALPHABETA with type jeu = Rep.jeu and  type coup = Rep.coup) = 
  struct
    let prof = ref 4
    exception Gagne
    exception Perd
    exception Nul
    let gagne = Aff.gagne
    let perd = Aff.perd
    let nul = Aff.nul
    let encore = Aff.q_continuer
    let le_jeu = ref (Rep.jeu_depart())
    let fin = Aff.fin
    let accueil = Aff.accueil 

    let tourH joueur () = 
      let choix = Aff.choix joueur !le_jeu in
      let ancien_jeu = !le_jeu 
      in le_jeu := Rep.jouer joueur choix !le_jeu ;
         Aff.affiche joueur choix ancien_jeu !le_jeu ;
         match Eval.etat_de joueur !le_jeu with
             Eval.P -> raise Perd
           | Eval.G -> raise Gagne
           | Eval.N -> raise Nul
           | _      -> ()

    let tourM joueur () = 
      let choix = Alpha.alphabeta !prof joueur !le_jeu in 
      let ancien_jeu = !le_jeu 
      in le_jeu := Rep.jouer joueur choix !le_jeu ;
         Aff.affiche joueur choix ancien_jeu !le_jeu ;
         match Eval.etat_de joueur !le_jeu with
             Eval.G -> raise Gagne
           | Eval.P -> raise Perd
           | Eval.N -> raise Nul
           | _      -> ()

    let init () =  
       let a = Aff.q_jouer () in
       let b = Aff.q_jouer() 
       in le_jeu :=Rep.jeu_depart () ;
          Aff.init () ; 
          match (a,b) with  
              true,true   -> tourM true, tourM false
            | true,false  -> tourM true, tourH false
            | false,true  -> tourH true, tourM false
            | false,false -> tourH true, tourH false
  end ;;


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
    in while !l > 0 && mat.(!l-1).(c-1) = Vide do  decr l done ; !l + 1

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
           if !y<0 || !y>=lig || !x<0 || !x>=col then raise Arg_invalid ; 
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
                        in v=plusI || v=moinsI || coups_legaux b m = []

  let est_stable b j = true

  type etat = G | P | N | C 

  let etat_de joueur m =
    let v = evaluer joueur m 
    in if v = plusI then if joueur then G else P
       else if v = moinsI then if joueur then P else G
       else if coups_legaux joueur m = [] then N else C
end ;;


(***************************************************************************)
(***          Jeux a deux joueurs : Puissance 4 - texte                  ***)  
(***************************************************************************)

(* open Alphabeta 
open P4
*)

module P4_text = struct
  open P4_rep
  type jeu = P4_rep.jeu
  type coup = P4_rep.coup

  let print_jeu mat = 
    for l = lig - 1 downto 0 do
      for c = 0 to col - 1 do
        match mat.(l).(c) with
            A    -> print_string "X " 
          | B    -> print_string "O "
          | Vide -> print_string ". "
      done; 
      print_newline ()
    done ;
    print_newline ()

  let accueil () = print_string "P4 ...\n"
  let fin () = print_string "A bientot ... \n"
  let question s = print_string s; print_string " o/n ? " ; read_line() = "o" 
  let q_commencer () = question "Voulez-vous commencer"
  let q_continuer () = question "Encore une partie"
  let q_jouer () = question "Est-ce une machine qui joue ?"

  let gagne  ()= print_string "Le 1er joueur gagne" ; print_newline ()
  let perd () = print_string "Le 1er joueur perd" ; print_newline ()
  let nul () = print_string "Partie nulle" ; print_newline ()
  
  let init () = print_string "X: 1er joueur   O: 2eme joueur"; 
                print_newline () ; print_newline () ;
                print_jeu (jeu_depart ()) ; print_newline()

  let affiche b c aj j = print_jeu j

  let est_coup = function '1'..'7' -> true | _ -> false

  exception Coup of int
  let rec choix joueur jeu =
    print_string ("Choix joueur" ^ (if joueur then "1" else "2") ^ " : ") ;
    let l = coups_legaux joueur jeu 
    in try  while true do 
              let i = read_line() 
              in ( if (String.length i > 0)  && (est_coup  i.[0])
                   then let c = (int_of_char i.[0]) - (int_of_char '0') 
                        in if List.mem c l then raise (Coup c) );
                 print_string "coup non valable, essayez encore : "
            done ; 
            List.hd l
       with  Coup i -> i 
         |    _     -> List.hd l
end ;;


module P4_squelette = 
  FSquelette (P4_rep) (P4_text) (P4_eval)  (FAlphabeta (P4_rep) (P4_eval)) ;;

module P4_main = FMain(P4_squelette) ;;

P4_main.main () ;;
