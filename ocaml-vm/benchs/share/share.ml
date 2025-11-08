
(* make vm SRC=benchs/share/share.ml *)

exception Identity ;;

let share f x = try f x with Identity -> x ;;

let filter f l =
  let rec fil l = match l with
    | [] -> raise Identity
    | h :: t -> if f h then h :: fil t else share fil t in
  share fil l ;;


let n = 20 ;; (* n=10 is for RTL simulation on PC,
                     augment n for FPGA or PC execution *)


  let l = List.init 500 (fun x -> x) in
  let l2 = filter (fun x -> x > 250) l in
  (*List.iter print_int l2;;*)
  ()



