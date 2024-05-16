open CustomStdlib ;;


(*   make vm SRC=benchs/list_length/length.ml *)

let length lst =
  let rec aux lst acc =
    match lst with
    | [] -> acc
    | _::t -> aux t (acc+1) in aux lst 0
;;

let rec interval n m =
  if n > m then [] else n :: interval (n+1) m ;;

let lst = interval 1 200;;

print_int 42;;

let nb_times = 20 ;; (* nb_times=1 is for RTL simulation on PC,
                        augment nb_times for FPGA or PC execution *)


for i = 1 to nb_times do
  print_int (length lst)
done ;;

