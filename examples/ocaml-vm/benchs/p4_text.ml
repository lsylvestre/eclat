(*****************************************************************************)
(*****  Devellopement d'applications avec Objective Caml                 *****)
(*****                                                                   *****)
(*****  Application : Jeux a deux joueurs : Puissance 4 - texte          *****)
(*****************************************************************************)



(***************************************************************************)
(***          Jeux a deux joueurs : Puissance 4 - texte                  ***)  
(***************************************************************************)

open Alphabeta 
open P4


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
