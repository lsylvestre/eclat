(*****************************************************************************)
(*****  Devellopement d'applications avec Objective Caml                 *****)
(*****                                                                   *****)
(*****  Application : évaluateur BASIC                                   *****)
(*****************************************************************************)

type op_unr = OPPOSE | NON  ;;

type op_bin = PLUS | MOINS | MULT | DIV | MOD  
            | EGAL | INF | INFEQ | SUP | SUPEQ | DIFF 
            | ET | OU  ;;

type expression = 
    ExpInt of int 
  | ExpVar of string
  | ExpStr of string 
  | ExpUnr of op_unr * expression
  | ExpBin of expression * op_bin * expression  ;;

type instruction = 
    Rem of string
  | Goto of int 
  | Print of expression
  | Input of string 
  | If of expression * int 
  | Let of string * expression  ;;

type ligne = { num : int ; inst : instruction }  ;;

type program = ligne list  ;;

type phrase =  Ligne of ligne | List | Run | End  ;;

(***************************************************************************)

let priority_ou = function NON -> 1 | OPPOSE -> 7
let priority_ob = function 
    MULT | DIV  -> 6
  | PLUS | MOINS -> 5
  | MOD -> 4
  | EGAL | INF | INFEQ | SUP | SUPEQ | DIFF -> 3
  | ET | OU -> 2 ;;

let pp_opbin = function  
    PLUS -> "+" | MULT -> "*" | MOD   -> "%" | MOINS -> "-" 
  | DIV -> "/"  | EGAL -> " = " | INF -> " < " 
  | INFEQ -> " <= "   | SUP   -> " > " 
  | SUPEQ -> " >= "   | DIFF  -> " <> " | ET -> " & " | OU -> " | "  
let pp_opunr = function OPPOSE -> "-"  | NON -> "!"  ;;

let parenthese x = "(" ^ x ^ ")";;  
let pp_expression = 
 let rec ppg pr = function 
      ExpInt n -> (string_of_int n)
    | ExpVar v -> v 
    | ExpStr s -> "\"" ^ s ^ "\"" 
    | ExpUnr (op,e) -> 
        let res = (pp_opunr op)^(ppg (priority_ou op) e) 
        in if pr=0 then res else parenthese res 
    | ExpBin (e1,op,e2) -> 
        let pr2 = priority_ob op
        in let res = (ppg pr2 e1)^(pp_opbin op)^(ppd pr2 e2)
        (* parenthèse si la priorité n'est pas supérieure *)
        in if pr2 >= pr then res else parenthese res
  and ppd pr exp = match exp with 
    (*  les sous-arbres droits ne diffèrent  *)
    (*  que pour les opérateurs binaires     *) 
      ExpBin (e1,op,e2) -> 
        let pr2 = priority_ob op
        in let res = (ppg pr2 e1)^(pp_opbin op)^(ppd pr2 e2)
        in if pr2 > pr then res else parenthese res
    | _ -> ppg pr exp
  in ppg 0 ;;

let pp_instruction = function 
    Rem s     ->  "REM " ^ s
  | Goto n    ->  "GOTO " ^ (string_of_int n)
  | Print e   ->  "PRINT " ^ (pp_expression e)
  | Input v   ->  "INPUT " ^ v
  | If (e,n)  ->  "IF "^(pp_expression e)^" THEN "^(string_of_int n)
  | Let (v,e) ->  "LET " ^ v ^ " = " ^ (pp_expression e)  ;;
let pp_ligne l = (string_of_int l.num) ^ "  " ^ (pp_instruction l.inst)  ;;

(***************************************************************************)

type lexeme = Lint of int 
            | Lident of string 
            | Lsymbol of string  
            | Lstring of string
            | Lfin ;;

type chaine_lexer = {chaine:string; mutable courant:int; taille:int } ;;

let init_lex s = { chaine=s; courant=0 ; taille=String.length s } ;;
let avance cl = cl.courant <- cl.courant+1  ;;
let avance_n cl n = cl.courant <- cl.courant+n ;;
let extrait pred cl = 
  let st = cl.chaine and ct = cl.courant in
  let rec ext n = if n<cl.taille && (pred st.[n]) then ext (n+1) else n in 
  let res = ext ct 
  in cl.courant <- res ; String.sub cl.chaine ct (res-ct)  ;;

let extrait_int = 
   let est_entier = function '0'..'9' -> true | _ -> false  
   in function cl -> int_of_string (extrait est_entier cl)
let extrait_ident =
   let est_alpha_num = function 
     'a'..'z' | 'A'..'Z' | '0' .. '9' | '_' -> true 
   | _ -> false  
   in extrait est_alpha_num ;;

exception LexerErreur ;;
let  rec lexer cl = 
  let lexer_char c = match c with 
      ' ' 
    | '\t'     -> avance cl ; lexer cl 
    | 'a'..'z' 
    | 'A'..'Z' -> Lident (extrait_ident cl)
    | '0'..'9' -> Lint (extrait_int cl)
    | '"'      -> avance cl ; 
                  let res = Lstring (extrait ((<>) '"') cl) 
                  in avance cl ; res 
    | '+' | '-' | '*' | '/' | '%' | '&' | '|' | '!' | '=' | '(' | ')'  -> 
                  avance cl; Lsymbol (String.make 1 c)
    | '<' 
    | '>'      -> avance cl; 
                  if cl.courant >= cl.taille then Lsymbol (String.make 1 c)
                  else  let cs = cl.chaine.[cl.courant] 
                        in ( match (c,cs) with 
		               ('<','=') -> avance cl; Lsymbol "<=" 
		      	     | ('>','=') -> avance cl; Lsymbol ">="
			     | ('<','>') -> avance cl; Lsymbol "<>"
			     |     _     -> Lsymbol (String.make 1 c) )
    | _ -> raise LexerErreur
  in 
     if cl.courant >= cl.taille then Lfin 
     else lexer_char cl.chaine.[cl.courant] ;;

(***************************************************************************)

type exp_elem = 
    Texp of expression   (* expression         *)
  | Tbin of op_bin       (* opérateur binaire  *)
  | Tunr of op_unr       (* opérateur unaire   *)
  | Tpg                  (* parenthèse gauche  *) ;;

exception ParseErreur ;;

let symb_unr = function 
  "!" -> NON | "-" -> OPPOSE | _ -> raise ParseErreur 

let symb_bin = function
    "+" -> PLUS | "-" -> MOINS | "*" -> MULT | "/" -> DIV | "%" -> MOD 
  | "=" -> EGAL | "<" -> INF | "<=" -> INFEQ | ">" -> SUP 
  | ">=" -> SUPEQ | "<>" -> DIFF | "&" -> ET | "|" -> OU 
  | _ -> raise ParseErreur 
let tsymb s = try Tbin (symb_bin s) with ParseErreur -> Tunr (symb_unr s) ;;

let reduit pr = function 
    (Texp e)::(Tunr op)::st  when  (priority_ou op) >= pr
       -> (Texp (ExpUnr (op,e)))::st
  | (Texp e1)::(Tbin op)::(Texp e2)::st  when  (priority_ob op) >= pr
       -> (Texp (ExpBin (e2,op,e1)))::st 
  | _ -> raise ParseErreur ;;

let rec empile_ou_reduit lex stack = match lex , stack with 
    Lint n ,  _      ->  (Texp (ExpInt n))::stack 
  | Lident v ,  _    ->  (Texp (ExpVar v))::stack 
  | Lstring s , _    ->  (Texp (ExpStr s))::stack
  | Lsymbol "(" , _  ->  Tpg::stack
  | Lsymbol ")" , (Texp e)::Tpg::st  ->  (Texp e)::st
  | Lsymbol ")" , _ -> empile_ou_reduit lex (reduit 0 stack) 
  | Lsymbol s , _ 
       -> let symbole = 
            if s<>"-" then tsymb s 
            (* lever l'ambiguïté du symbole ``-''            *)
            (* suivant la pile (i.e dernier exp_elem empilé) *) 
            else match stack 
                 with (Texp _)::_  ->  Tbin MOINS 
                               | _ ->  Tunr OPPOSE 
          in ( match symbole with 
                 Tunr op  ->  (Tunr op)::stack 
               | Tbin op  -> 
                   ( try empile_ou_reduit lex (reduit (priority_ob op) 
                                              stack )
                     with ParseErreur -> (Tbin op)::stack )
               | _ -> raise ParseErreur )
  | _ , _ -> raise ParseErreur ;;

let rec reduit_tout = function 
  | [] -> raise ParseErreur
  | [Texp x] -> x 
  | st -> reduit_tout (reduit 0 st) ;;

let parse_exp fin cl = 
  let p = ref 0 
  in let rec parse_un stack  = 
       let l = ( p:=cl.courant ; lexer cl)
        in if not (fin l) then parse_un (empile_ou_reduit l stack)
           else ( cl.courant <- !p ; reduit_tout stack )
  in parse_un []  ;;

let parse_inst cl = match lexer cl with 
    Lident s -> ( match s with 
        "REM" -> Rem (extrait (fun _ -> true) cl)
      | "GOTO" -> Goto (match lexer cl with 
                          Lint p -> p 
                        | _ -> raise ParseErreur)
      | "INPUT" -> Input (match lexer cl with 
                            Lident v -> v 
                          | _ -> raise ParseErreur)
      | "PRINT" -> Print (parse_exp ((=) Lfin) cl)
      | "LET" -> 
          let l2 = lexer cl and l3 = lexer cl 
          in ( match l2 ,l3 with 
                   (Lident v,Lsymbol "=") -> Let (v,parse_exp ((=) Lfin) cl)
                 | _ -> raise ParseErreur )
      | "IF" -> 
          let test = parse_exp ((=) (Lident "THEN")) cl 
          in ( match ignore (lexer cl) ; lexer cl with 
                  Lint n  ->  If (test,n) 
                | _ -> raise ParseErreur )
      | _ -> raise ParseErreur ) 
  | _ -> raise ParseErreur  ;;

let parse str = 
  let cl = init_lex str 
  in match lexer cl with 
      Lint n -> Ligne { num=n ; inst=parse_inst cl }
    | Lident "LIST" -> List 
    | Lident "RUN" -> Run 
    | Lident "END" -> End 
    | _ -> raise ParseErreur  ;;

(***************************************************************************)

type valeur = Vint of int | Vstr of string | Vbool of bool  ;;

type environnement = (string * valeur) list ;;

type code = ligne array ;;

type etat_exec = { ligne:int ; xprog:code ; xenv:environnement } ;;

exception RunErreur of int 
let runerr n = raise (RunErreur n) ;;

exception Resultat_cherche_indice of int ;;
let cherche_indice tprog num_ligne =
  try 
    for i=0 to (Array.length tprog)-1 do 
      let mun_i = tprog.(i).num
      in if mun_i=num_ligne then raise (Resultat_cherche_indice i)
         else if mun_i>num_ligne then raise (Resultat_cherche_indice (-1))
    done ;
    (-1 )
  with Resultat_cherche_indice i -> i ;;

let assemble prog =
  let tprog = Array.of_list prog in
    for i=0 to (Array.length tprog)-1 do
      match tprog.(i).inst with
          Goto n -> let indice = cherche_indice tprog n
                    in tprog.(i) <- { tprog.(i) with inst = Goto indice }
        | If(c,n) -> let indice = cherche_indice tprog n
                     in tprog.(i) <- { tprog.(i) with inst = If (c,indice) }
        | _ -> ()
    done ;
  tprog ;;

let rec eval_exp n envt expr = match expr with 
    ExpInt p  ->  Vint p
  | ExpVar v  -> ( try List.assoc v envt with Not_found -> runerr n )
  | ExpUnr (OPPOSE,e) ->  
      ( match eval_exp n envt e with 
          Vint p -> Vint (-p) 
        | _ -> runerr n )
  | ExpUnr (NON,e) ->  
      ( match eval_exp n envt e with 
          Vbool p -> Vbool (not p) 
        | _ -> runerr n )
  | ExpStr s -> Vstr s  
  | ExpBin (e1,op,e2) 
      -> match eval_exp n envt e1 , op , eval_exp n envt e2 with
              Vint v1 , PLUS  , Vint v2  ->  Vint (v1 + v2) 
            | Vint v1 , MOINS , Vint v2  ->  Vint (v1 - v2) 
            | Vint v1 , MULT  , Vint v2  ->  Vint (v1 * v2) 
            | Vint v1 ,  DIV  , Vint v2  when v2<>0 ->  Vint (v1 / v2) 
            | Vint v1 ,  MOD  , Vint v2  when v2<>0 ->  Vint (v1 mod v2) 

            | Vint v1 , EGAL  , Vint v2  ->  Vbool (v1 = v2) 
            | Vint v1 , DIFF  , Vint v2  ->  Vbool (v1 <> v2) 
            | Vint v1 ,  INF  , Vint v2  ->  Vbool (v1 < v2) 
            | Vint v1 ,  SUP  , Vint v2  ->  Vbool (v1 > v2) 
            | Vint v1 , INFEQ , Vint v2  ->  Vbool (v1 <= v2) 
            | Vint v1 , SUPEQ , Vint v2  ->  Vbool (v1 >= v2) 

            | Vbool v1 , ET , Vbool v2  ->  Vbool (v1 && v2) 
            | Vbool v1 , OU , Vbool v2  ->  Vbool (v1 || v2) 

            | Vstr v1 , PLUS , Vstr v2 -> Vstr (v1 ^ v2) 
            | _ , _ , _  -> runerr n  ;;

let rec ajoute v e env = match env with 
    [] -> [v,e]
  | (w,f)::l -> if w=v then (v,e)::l else (w,f)::(ajoute v e l) ;;

let print_valeur v = match v with 
    Vint n -> print_int n
  | Vbool true -> print_string "true"
  | Vbool false -> print_string "false"
  | Vstr s -> print_string s ;;

let ligne_suivante etat =
 let n = etat.ligne+1 in
 if n < Array.length etat.xprog then n else -1 ;;

let eval_inst etat =
  match etat.xprog.(etat.ligne).inst with 
    Rem _    ->  { etat with ligne = ligne_suivante etat }
  | Print e  ->  print_valeur (eval_exp etat.ligne etat.xenv e) ; 
                 print_newline () ;
                 { etat with ligne = ligne_suivante etat }
  | Let(v,e) ->  let ev = eval_exp etat.ligne etat.xenv e 
                   in { etat with ligne = ligne_suivante etat ; 
                                  xenv = ajoute v ev etat.xenv }
  | Goto n   ->  { etat with ligne = n }
  | Input v  ->  let x = try read_int () 
                         with Failure "int_of_string" -> 0
                 in { etat with ligne = ligne_suivante etat; 
                                xenv = ajoute v (Vint x) etat.xenv }
  | If (t,n) ->  match eval_exp etat.ligne etat.xenv t with 
                   Vbool true  -> { etat with ligne = n }
                 | Vbool false -> { etat with ligne = ligne_suivante etat }
                 | _ -> runerr etat.ligne  ;;

let rec run etat = 
 if etat.ligne = -1 then etat else run (eval_inst etat) ;;

(***************************************************************************)


let rec inserer ligne p = match p with  
    [] -> [ligne]
  | l::prog -> 
      if l.num < ligne.num then l::(inserer ligne prog)
      else if l.num=ligne.num then ligne::prog
      else ligne::l::prog ;;

let print_prog prog = 
  let print_ligne x = print_string (pp_ligne x) ; print_newline () in
   print_newline () ;
   List.iter print_ligne prog ;
   print_newline () ;;

type etat_boucle = { prog:program; env:environnement } ;;

exception Fin ;;
let une_commande etat =
  print_string "> " ; flush stdout ;
  try 
    match parse (input_line stdin) with 
       Ligne l -> { etat with prog = inserer l etat.prog }
     | List  -> (print_prog etat.prog ; etat )
     | Run 
      -> let tprog = assemble etat.prog in
         let xetat = run { ligne = 0; xprog = tprog; xenv = etat.env } in
          {etat with env = xetat.xenv }
     | End -> raise Fin
  with 
      LexerErreur -> print_string "Illegal character\n"; etat 
    | ParseErreur -> print_string "syntax error\n"; etat 
    | RunErreur n -> 
        print_string "runtime error at line ";
        print_int n ;
        print_string "\n";  
        etat ;;

let go () = 
try 
  print_string "Mini-BASIC version 0.1\n\n";
  let rec loop etat = loop (une_commande etat)  in 
    loop { prog = []; env = [] }
with Fin -> print_string "A bientôt...\n";;

go () ;;




