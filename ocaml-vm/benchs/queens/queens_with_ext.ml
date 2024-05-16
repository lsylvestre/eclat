open CustomStdlib ;;

(* make vm SRC=benchs/queens/queens_with_ext.ml CUSTOM="benchs/queens/queens_ext.ecl" *)

let rec interval n m =
  if n > m then [] else n :: interval (n+1) m ;;

let rec concmap f l =
  match l with
  | [] -> []
  | x :: l -> f x @ concmap f l ;;

external ok : int list -> bool = "ok" ;;

let range = interval 1 ;;

(* redefine [List.map] as a tail-recursive function *)
let map f l =
  let rec aux(l,acc) =
    match l with
    | [] -> List.rev acc
    | x::l -> let y = f x in aux(l,y::acc) in aux(l,[]) ;;

let queens n =
  let qs = range n in
  let testcol b = List.filter ok (map (fun q -> q::b) qs) in
  let rec gen = function
    | 0 -> [[]]
    | n -> concmap testcol (gen (n - 1)) in
  List.length (gen n) ;;

let nb_times = 1 ;; (* nb_times=1 is for RTL simulation on PC,
                       augment nb_times for FPGA or PC execution *)

for i = 1 to nb_times do
  print_int (queens 7)
done ;; 
 