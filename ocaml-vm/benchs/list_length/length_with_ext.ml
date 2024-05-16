open CustomStdlib ;;

(*  make vm SRC=benchs/list_length/length_with_ext.ml   \
            CUSTOM="benchs/list_length/length_ext.ecl" FLAGS=""

*)


external length : 'a list -> int = "length_ext" ;;

let rec interval n m =
  if n > m then [] else n :: interval (n+1) m ;;

let l = interval 1 200;;

print_int 42;;


let nb_times = 1 ;; (* nb_times=1 is for RTL simulation on PC,
                       augment nb_times for FPGA or PC execution *)

for i = 1 to nb_times do
  print_int (length l)
done ;;



