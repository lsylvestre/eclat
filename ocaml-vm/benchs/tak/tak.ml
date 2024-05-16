open CustomStdlib ;;

(* make vm SRC=benchs/tak/tak.ml *)

let rec tak (x, y, z) =
  if x > y then tak (tak (x - 1, y, z), tak (y - 1, z, x), tak (z - 1, x, y))
  else z ;;

let nb_times = 1 ;; (* nb_times=1 is for RTL simulation on PC,
                       augment nb_times for FPGA or PC execution *)

for i = 1 to nb_times do
  print_int (tak(18, 12, 6))
done ;;
