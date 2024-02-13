open CustomStdlib

(* binary search tree *)

type bs_tree = Empty | Node of int * bs_tree * bs_tree

let rec search n t =
  match t with
  | Empty -> false
  | Node(x,l,r) -> if n < x then search n l else
                   if n > x then search n r
                            else true
;;

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
     print_int (if search 0 (comb n)   (* 0 is not in the tree *)
                then 1
                else 0)
  done ;;

print_int 42;;
