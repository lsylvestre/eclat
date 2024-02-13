open CustomStdlib

(*   make vm SRC=benchs/bst/bst_with_ext.ml CUSTOM=benchs/bst/bst_ext.ecl   *)


(* binary search tree *)

type bs_tree = Empty | Node of int * bs_tree * bs_tree

external search : int -> bs_tree -> bool = "search_ext" ;;

let rec comb n =
  if n <= 0 then Empty 
  else Node(n,comb (n-1),Empty)
   
;;


let nb_times = 20 ;; (* nb_times=20 is for RTL simulation on PC,
                        augment nb_times for FPGA or PC execution *)

let n = 200 ;;

print_int 5 ;;

let () = 
  for i = 1 to nb_times do
     print_int (if search 0 (comb n)  (* 0 is not in the tree *)
                then 1 
                else 0) 
  done ;;

print_int 42;;