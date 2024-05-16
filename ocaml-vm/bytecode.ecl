(* THIS FILE HAS BEEN GENERATED *)

let init_data () =
let x1 = global_start in
(* ADD GLOBAL 12 *)
ram.(global_start + 12) <- val_long(0);
(* ADD GLOBAL 13 *)
ram.(global_start + 13) <- val_long(0);
  global_end.(0) <- global_start + 14
 ;;

let external_call (n,args,env) =
  match n with
  | 0 -> caml_print_int(args,env)
  | _ -> print_string "unknown primitive"; (val_unit,env)
  end ;;


let static code = (0:long)^84 ;;

let load_code () =
  code.(0) <- 84;
  code.(1) <- 29;
  code.(2) <- 0;
  code.(3) <- 127;
  code.(4) <- 1;
  code.(5) <- 40;
  code.(6) <- 1;
  code.(7) <- 0;
  code.(8) <- 54;
  code.(9) <- 12;
  code.(10) <- 54;
  code.(11) <- 12;
  code.(12) <- 38;
  code.(13) <- 3;
  code.(14) <- 0;
  code.(15) <- 54;
  code.(16) <- 13;
  code.(17) <- 54;
  code.(18) <- 13;
  code.(19) <- 38;
  code.(20) <- 3;
  code.(21) <- 41;
  code.(22) <- 42;
  code.(23) <- 1;
  code.(24) <- 1;
  code.(25) <- 11;
  code.(26) <- 33;
  code.(27) <- 11;
  code.(28) <- 37;
  code.(29) <- 3;
  code.(30) <- 43;
  code.(31) <- 0;
  code.(32) <- -10;
  code.(33) <- 9;
  code.(34) <- 57;
  code.(35) <- 13;
  code.(36) <- 0;
  code.(37) <- 19;
  code.(38) <- 1;
  code.(39) <- 9;
  code.(40) <- 43;
  code.(41) <- 0;
  code.(42) <- -28;
  code.(43) <- 57;
  code.(44) <- 12;
  code.(45) <- 43;
  code.(46) <- 0;
  code.(47) <- -40;
  code.(48) <- 9;
  code.(49) <- 43;
  code.(50) <- 0;
  code.(51) <- -49;
  code.(52) <- 105;
  code.(53) <- 105;
  code.(54) <- 11;
  code.(55) <- 9;
  code.(56) <- 12;
  code.(57) <- 125;
  code.(58) <- 85;
  code.(59) <- 20;
  code.(60) <- 92;
  code.(61) <- 100;
  code.(62) <- 14;
  code.(63) <- 16;
  code.(64) <- 18;
  code.(65) <- 8;
  code.(66) <- 35;
  code.(67) <- 93;
  code.(68) <- 0;
  code.(69) <- 1;
  code.(70) <- 9;
  code.(71) <- 127;
  code.(72) <- 1;
  code.(73) <- 20;
  code.(74) <- 2;
  code.(75) <- 1;
  code.(76) <- 122;
  code.(77) <- 85;
  code.(78) <- -18;
  code.(79) <- 19;
  code.(80) <- 2;
  code.(81) <- 19;
  code.(82) <- 4;
  code.(83) <- 143;
  ()
 ;;

