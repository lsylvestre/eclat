(* THIS FILE HAS BEEN GENERATED *)

let init_data () =
let x1 = global_start in
  global_end.(0) <- global_start + 12
 ;;

let external_call (n,args,env) =
  match n with
  | 0 -> caml_print_int(args,env)
  | _ -> print_string "unknown primitive"; (val_unit,env)
  end ;;


let static code = (0:long)^35 ;;

let load_code () =
  code.(0) <- 84;
  code.(1) <- 21;
  code.(2) <- 0;
  code.(3) <- 134;
  code.(4) <- 2;
  code.(5) <- 4;
  code.(6) <- 100;
  code.(7) <- 40;
  code.(8) <- 1;
  code.(9) <- 0;
  code.(10) <- 127;
  code.(11) <- -2;
  code.(12) <- 50;
  code.(13) <- 33;
  code.(14) <- 11;
  code.(15) <- 127;
  code.(16) <- -1;
  code.(17) <- 50;
  code.(18) <- 33;
  code.(19) <- 110;
  code.(20) <- 40;
  code.(21) <- 1;
  code.(22) <- 44;
  code.(23) <- 1;
  code.(24) <- 0;
  code.(25) <- -23;
  code.(26) <- 103;
  code.(27) <- 11;
  code.(28) <- 11;
  code.(29) <- 33;
  code.(30) <- 93;
  code.(31) <- 0;
  code.(32) <- 19;
  code.(33) <- 1;
  code.(34) <- 143;
  ()
 ;;

