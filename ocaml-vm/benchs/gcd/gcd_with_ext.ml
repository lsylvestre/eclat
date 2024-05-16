open CustomStdlib

(*  make vm SRC=benchs/gcd/gcd_with_ext.ml CUSTOM=benchs/gcd/gcd_ext.ecl *)

external gcd : int -> int -> int = "gcd_ext" ;;

let n = 50_000 ;; (* n=50_000 is for RTL simulation on PC,
                     augment n for FPGA or PC execution *)
let () =
  print_int 42;
  print_int (gcd 1 n)
