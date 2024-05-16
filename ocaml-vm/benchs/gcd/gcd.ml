open CustomStdlib

(*  make vm SRC=benchs/gcd/gcd.ml *)

let rec gcd a b =
  if a < b then gcd a (b-a) else
  if a > b then gcd (a-b) b
  else a ;;

let n = 50_000 ;; (* n=50_000 is for RTL simulation on PC,
                     augment n for FPGA or PC execution *)
let () =
  print_int 42;
  print_int (gcd 1 n)
