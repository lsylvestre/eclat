(*********************************************************************)
(*                                                                   *)
(*                     Advent Of Code 2025                           *)
(*                                                                   *)
(*           puzzle 2 - task 1 & 2 - Eclat version                   *)
(*                                                                   *)
(*********************************************************************)
(*                                                                   *)
(* author : Emmanuel Chailloux                                       *)
(*                                                                   *)
(*********************************************************************)

(* global variables *)

(* debug mode *)
let debug = false ;;
let debug2 = true ;;

(* original ocaml code 

   let global_command = ref 0 ;;
   let global_invalid = ref 0 ;;
   let total_invalid = ref 0 ;;
 *)

let global_command : int<48> array<1> = create<1> () ;;
let global_invalid : int<48> array<1> = create<1> () ;;
let total_invalid  : int<48> array<1> = create<1> () ;;

let init_globals_vars () = 
  set(global_command,0,0);
  set(global_invalid,0,0);
  set(total_invalid,0,0);
  ();;

(* powers of 10  *)

let power_10 : int<48> array<13>  = create<13> () ;;
let fois_10 x  : int<48> = x + x + x + x + x + x + x + x + x + x ;;

let init_power_10 (n : int<48>)   =  
  set(power_10,0,1);
  let rec aux i = 
    if i < n then (
           set(power_10, i, fois_10 (get(power_10,(i-1))));
           aux (i+1) 
    ) 
    else  () 
  in
    aux(1) 
;;

(* init_power_10 (12) : in the body of the main function  *)


(* naive version of integer division *)
let rec division_entiere_aux (a, b, q) = 
  if a < b then (q,a)
  else division_entiere_aux ((a-b), b, (q+1))
;;


let division_entiere (a,b) = division_entiere_aux (a,b,0) 
;;


(*                                          *)
(* the slow factor actually comes from here *)
(*                                          *)

(* 
  let division_entiere ( (a,b) : int<48> * int<48>) =  
  a/b, a mod b ;;
*) 

 
(* initial check for the first task *)
let check (id, d) =
  let (a,b) = division_entiere(id, d) in
  a == b 
;;

(* check equality for n numbers n in ID  *)
let rec equal_pack_aux ( (id, r, d, n) : int<48> * int<48> * int<48> * int<48> ) =
  if n == 1 then id == r
  else
    let (a,b) = division_entiere(id, d) in
      if r == b then equal_pack_aux (a, r, d, (n-1)) else false
;;

let equal_pack (id, d, n, l) =
  let (a,b) = division_entiere(id, d) in  equal_pack_aux (a, b, d, ((l/n)-1))
;;

(* next divisor until 1 *)
let rec next_divisor (i, n) =
if i == 1 then i
else 
 let (a,b) = division_entiere (n, i) in
  if b == 0 then i
  else
    next_divisor ((i-1), n) 
;;

(* checks validity by slice *)
let rec check2 (id, d, l) =
  if d < 1 then false 
  else
    let new_d = next_divisor (d, l) in
      if equal_pack (id, get(power_10,(new_d)), new_d, l) then true
      else check2 (id, (d-1),  l)
;;

(* main function to check if an ID is invalid *)
let is_invalid id =
if id < 10 then false
else if id < 100 then           check2 (id, 1, 2)    (* 1 1 *)  
else if id < 1000 then          check2 (id, 1, 3)    (* 1 1 1 *)
else if id < 10000 then         check2 (id, 2, 4)    (* 2 2 | 1 1 1 1 *)
else if id < 100000 then        check2 (id, 1, 5)    (* 1 1 1 1 1 *)
else if id < 1000000 then       check2 (id, 3, 6)    (* 3 3 | 2 2 2 | 1 1 1 1 1 1 *)
else if id < 10000000 then      check2 (id, 1, 7)    (* 1 1 1 1 1 1 1 *)
else if id < 100000000 then     check2 (id, 4, 8)    (* 4 4 | 2 2 2 2 | 1 1 1 1 1 1 1 1 *)
else if id < 1000000000 then    check2 (id, 3, 9)    (* 3 3 3 | 1 1 1 1 1 1 1 1 1 *)
else if id < 10000000000 then   check2 (id, 5, 10)   (* 5 5 | 2 2 2 2 2 | 1 1 1 1 1 1 1 1 1 1 1 *)
else if id < 100000000000 then  check2 (id, 1, 11)   (* 1 1 1 1 1 1 1 1 1 1 1 *)
else if id < 1000000000000 then check2 (id, 6, 12)   (* 6 6 | 4 4 4 | 3 3 3 3 | 2 2 2 2 2 2 | 1 1 1 1 1 1 1 1 1 1 1 1 *) 
else ( print_string "PB is_invalid : too large  number" ; false) 
;;

let incr_step ((r, s) : int<48> array<1> * int<48>)  = set(r,0,get(r,0)+s) ;;
let incr (r : int<48> array<1> )  = incr_step(r,1) ;;

(* computes sum of invalid IDs inside an interval *)
let rec interval  (a, b) =
  if debug2 then (print_int a; print_string "-->"; print_int b; print_newline());
  if a <= b then (
    incr global_command ;
    ( if is_invalid a then (
        incr global_invalid ;
        incr_step(total_invalid,a) ;
        if debug then (
          print_string " : true " ; 
          print_int (get(global_invalid,0))
        )
      )
    );
    if debug then (print_newline());
    interval ((a+1), b)
  )  
;;

(* 
let input0 = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,\
              1698522-1698528,446443-446449,38593856-38593862,565653-565659,\
              824824821-824824827,2121212118-2121212124"
;;

création à la main de l'entrée input0.txt : 11 intervalles 

*)
let dim :  int<48> = 11 ;;
type interval = int<48> * int<48> ;;
let input0 : interval  array<'N> = create<11> () ;;

let int_of_bytes ((by,i)  :  interval  array<'N> * int<48> array<1>) =
  let rec aux(n) = 
    let token_digit = 
      resize_int<48>(char_code(bytes_get(by,get(i,0))) - 48 )
    in 
      incr i;
      if ((token_digit < 0) or (token_digit > 9)) then n 
      else aux(10*n + token_digit) 
  in
    aux(0)
;; 

let next_token(by,i) =
  if debug then (
    print_string "pos : ";print_int (get(i,0)) ; print_string ".. "
  ) ;
  let a = int_of_bytes(by,i) in 
  let b =  int_of_bytes(by,i) in 
    if debug then (
      print_string "$"; print_int (a) ; print_string "-"; 
      print_int b; print_newline()) ;
    (a,b)
;;

let init_input (by) = 
  let i = create<1> () in
  let j = create<1> () in  
    set(i,0,0);
    set(j,0,0);
    let rec aux() = 
      if get(j,0) < dim then (
        let t = next_token (by,i) in 
          set(input0,
              get(j,0),
              t);
          incr j ;
          aux () 
      )
      else (if debug then (print_string "end of input initialization"; print_newline()))
    in aux () 
;;

(*
let init_input () = 
  set(input0,0,(11,22));
  set(input0,1,(95,115));
  set(input0,2,(998,1012));
  set(input0,3,(1188511880,1188511890));
  set(input0,4,(222220,222224)); 
  set(input0,5,(1698522,1698528));
  set(input0,6,(446443,446449));
  set(input0,7,(38593856,38593862));
  set(input0,8,(565653,565659));
  set(input0,9,(824824821,824824827));
  set(input0,10,(2121212118,2121212124));
  ()
;;
*)

let print_input () = 
  print_string "input : "; print_int dim; 
  print_string " lines"; print_newline();
  for i = 0 to dim - 1 do 
    let (a,b) = get(input0,i) in 
      print_string "["; print_int a; print_string", "; 
      print_int b; print_string "]"; print_newline()
  done ; 
  print_string "----"; print_newline();
  ()
;;

let resolve n = 
  let rec aux i = 
    if i < n then (
        let (a,b) = get(input0,i) in 
        interval(a,b) ;
        aux (i+1)
    )
    else ()
  in  aux(0) 
;;

let print_results () = 
  print_string "global commands : "; 
  print_int (get(global_command,0)) ; 
  print_newline() ;
  print_string "global_invalid : " ; 
  print_int (get(global_invalid,0)) ; 
  print_newline() ;
  print_string "total_invalid : " ; 
  print_int (get(total_invalid,0)) ; 
  print_newline()
;;


(* cycle counter *)
let chrono () = 
  reg (fun c -> c + 1) init (-1) ;;


let main () = 
  let cy = chrono() in  
    reg (fun ((),rdy) ->
      if rdy then
        let _ = 
          exec 
            print_string "execution time (nb cycles): ";
            print_int cy;
            print_newline ();
            let rec fini() = fini() 
            in fini()
          default ()
        in ((),true)
      else
        exec
          init_globals_vars();
          init_power_10 (12);
          print_string "reading the file `input.txt`";
          print_newline ();
            let by = (input_file "puzzle2-input0.txt" : bytes<162> (* <*1868> <38> *)) in
              if debug then (bytes_print by ; print_newline());
              init_input (by);
              print_input();
              resolve(11);
              print_results()
        default ()) 
    init ((),false) 
;;

(*
let main2 () =
  init_globals();
  init_power_10 (12);
  init_input ();
  print_input();
  compute(11);
  print_results();
  ()
;;
*)

(*
let main () = 
 exec my_main () default () 
;;
*)

