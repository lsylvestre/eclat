(* THIS FILE HAS BEEN GENERATED *)

let external_call (n,args,env) =
  match n with
  | 0 -> caml_identity(args,env)
  | 1 -> caml_sub(args,env)
  | 2 -> caml_mult(args,env)
  | 3 -> caml_div(args,env)
  | 4 -> caml_mod(args,env)
  | 5 -> caml_and(args,env)
  | 6 -> caml_or(args,env)
  | 7 -> caml_xor(args,env)
  | 8 -> caml_lsl(args,env)
  | 9 -> caml_lsr(args,env)
  | 10 -> caml_asr(args,env)
  | 11 -> caml_not(args,env)
  | 12 -> caml_neg(args,env)
  | 13 -> caml_is_int(args,env)
  | 14 -> caml_vectlength(args,env)
  | 15 -> caml_fresh_oo_id(args,env)
  | 16 -> caml_print_int(args,env)
  | 17 -> caml_print_string(args,env)
  | 18 -> caml_print_int(args,env)
  | 19 -> caml_led_off(args,env)
  | 20 -> caml_led_on(args,env)
  | 21 -> caml_equal(args,env)
  | 22 -> caml_lessthan(args,env)
  | 23 -> caml_greaterthan(args,env)
  | 24 -> caml_greaterequal(args,env)
  | 25 -> caml_lessequal(args,env)
  | 26 -> caml_fresh_oo_id(args,env)
  | 27 -> caml_make_vect(args,env)
  | 28 -> caml_obj_dup(args,env)
  | 29 -> bytes_create(args,env)
  | 30 -> caml_array_get(args,env)
  | 31 -> caml_array_set(args,env)
  | 32 -> caml_array_unsafe_set(args,env)
  | 33 -> caml_array_unsafe_get(args,env)
  | 34 -> caml_array_get_addr(args,env)
  | 35 -> caml_array_set_addr(args,env)
  | 36 -> caml_array_unsafe_get_addr(args,env)
  | 37 -> caml_array_unsafe_set_addr(args,env)
  | 38 -> unsafe_chr(args,env)
  | 39 -> bytes_unsafe_set(args,env)
  | 40 -> unsafe_to_string(args,env)
  | 41 -> caml_ml_string_length(args,env)
  | 42 -> caml_create_bytes(args,env)
  | 43 -> caml_blit_string(args,env)
  | 44 -> caml_string_of_bytes(args,env)
  | 45 -> caml_compare(args,env)
  | 46 -> caml_array_sub(args,env)
  | 47 -> caml_string_get(args,env)
  | 48 -> caml_string_equal(args,env)
  | 49 -> caml_print_newline(args,env)
  | _ -> print_string "unknown primitive"; (val_unit,env)
  ;;


let static code = (GROUP3(STOP()):opcode)^4096 ;;
let static data_rom = ((0,false))^2048 ;;


 let load_code () =
  (code.(0) <- CONST(0));
  (code.(1) <- GROUP4(CALL(26,false,false,false,false)));
  (code.(2) <- CONST(0));
  (code.(3) <- GROUP4(CALL(26,false,false,false,false)));
  (code.(4) <- GROUP3(BRANCH(22)));
  (code.(5) <- ACC(1));
  (code.(6) <- GROUP_COMPARE(LE(),BCOMPARE(2,9)));
  (code.(7) <- CONST(1));
  (code.(8) <- GROUP4(RETURN(1)));
  (code.(9) <- ACC(1));
  (code.(10) <- ADD(true,-2));
  (code.(11) <- PUSH());
  (code.(12) <- OFFSETCLOSURE(0));
  (code.(13) <- GROUP4(APPLY(true,1,0)));
  (code.(14) <- PUSH());
  (code.(15) <- ACC(2));
  (code.(16) <- ADD(true,-1));
  (code.(17) <- PUSH());
  (code.(18) <- OFFSETCLOSURE(0));
  (code.(19) <- GROUP4(APPLY(true,1,0)));
  (code.(20) <- ADD(false,0));
  (code.(21) <- GROUP4(RETURN(1)));
  (code.(22) <- GROUP4(CLOSUREREC(1,0,31,5)));
  (code.(23) <- CONST(42));
  (code.(24) <- GROUP4(CALL(18,false,false,false,false)));
  (code.(25) <- CONST(7));
  (code.(26) <- PUSH());
  (code.(27) <- ACC(2));
  (code.(28) <- GROUP4(APPLY(true,1,0)));
  (code.(29) <- POP(1));
  (code.(30) <- GROUP3(STOP()));
  (code.(31) <- LABEL(5));
  ()
 ;;

let init_data () = (
let x1 = global_start + 12 in
  for i = 0 to 2047 do
    ram.(i) <- data_rom.(i)
  done ) ;;
let main_load () =
  load_code();
  init_data();
  global_end.(0) <- global_start + 12 ;;

