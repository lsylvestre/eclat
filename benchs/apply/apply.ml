open CustomStdlib ;;


(* make vm SRC=benchs/apply/apply.ml *)


let double = fun f x -> f (f x)
let quad x  = double double  x
let oct x = quad quad x
let succ x = x + 1 ;;

let nb_times = 1 ;; (* nb_times=1 is for RTL simulation on PC,
                       augment nb_times for FPGA or PC execution *)

for i = 1 to nb_times do
  print_int (double oct succ 1)
done ;;
