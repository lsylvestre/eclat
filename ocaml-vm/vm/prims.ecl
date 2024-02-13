(* ********************************************** *)

let caml_prepare_args1(v) =
  (v,(val_unit,(val_unit,(val_unit,val_unit)))) ;;

let caml_prepare_args2(v1,v2) =
  (v1,(v2,(val_unit,(val_unit,val_unit)))) ;;

let caml_prepare_args3(v1,v2,v3) =
  (v1,(v2,(v3,(val_unit,val_unit)))) ;;

let caml_prepare_args4(v1,v2,v3,v4) =
  (v1,(v2,(v3,(v4,val_unit)))) ;;


let caml_prepare_args5(v1,v2,v3,v4,v5) =
  (v1,(v2,(v3,(v4,v5)))) ;;

(* **** example of external function **** *)

let caml_identity(arg,st) = (arg,st) ;;

(* *********** displaying *************** *)

let caml_print_int ((v1,_),st) =
  print_string "======> ";
  print_int (long_val v1);
  print_newline ();
  (val_unit,st) ;;

(* *********** controlling a LED *************** *)

let caml_led_off (v,(pc,acc,sp,regs,(finished,_))) =
  (val_unit,(pc,acc,sp,regs,(finished,true))) ;;

let caml_led_on (v,(pc,acc,sp,regs,(finished,_))) =
  (val_unit,(pc,acc,sp,regs,(finished,false))) ;;

(* *********** comparisons *************** *)

let caml_equal ((v1,(v2,_)),st) =
  let b = long_val(v1) = long_val(v2) in
  let v = val_long(int_of_bool(b)) in
  (v,st) ;;

let caml_lessthan ((v1,(v2,_)),st) =
  let b = long_val(v1) < long_val(v2) in
  let v = val_long(int_of_bool(b)) in
  (v,st) ;;

let caml_greaterthan ((v1,(v2,_)),st) =
  let b = long_val(v1) > long_val(v2) in
  let v = val_long(int_of_bool(b)) in
  (v,st) ;;

let caml_greaterequal ((v1,(v2,_)),st) =
  let b = long_val(v1) >= long_val(v2) in
  let v = val_long(int_of_bool(b)) in
  (v,st) ;;

let caml_lessequal ((v1,(v2,_)),st) =
  let b = long_val(v1) <= long_val(v2) in
  let v = val_long(int_of_bool(b)) in
  (v,st) ;;

(* *********** runtime *************** *)

let caml_fresh_oo_id(_,st) =
  let gensym n = n + 1 in
  (val_long (reg gensym last 0),st) ;;

let caml_obj_dup((arg,_),st) =
  let sz = size_val(arg) in
  if sz == 0 then (arg,st) else (
  let tag = char_of_short(tag_val arg) in
  
  let (pc,acc,sp, (env, extra_args, trap_sp), others) = st in
  
  let (acc,env,blk) = make_block(tag,sz) in
  
  let st = (pc,acc,sp, (env, extra_args, trap_sp), others) in
  
  let rec w(i) =
    if i >= sz then () else
    (set_field(blk,i,get_field(arg,i)); w(i+1))
  in
  w(0);
  (blk,st)) ;;


(******************** arrays ********************)

let bound_error s =
  fatal_error "index out of bounds" ;;

let caml_make_vect ((v_size,(v_init,_)),st) =
  let sz = as_short(long_val(v_size)) in

  let (pc,acc,sp, (env, extra_args, trap_sp), others) = st in

  let (acc,env,blk) = make_block(sp,acc,env,0,sz) in

  let st = (pc,acc,sp, (env, extra_args, trap_sp), others) in
  let rec w(i) =
    if i >= sz then () else
    (set_field(blk,i,v_init); w(i+1))
  in
  w(0);
  (blk,st) ;;


let caml_array_get((v_array,(v_index,_)),st) =
  let idx = as_short(long_val(v_index)) in
  if (idx < 0) or (idx >= size_val(v_array)) then bound_error () else
  let v = get_field(v_array, idx) in
  (v,st) ;;


let caml_array_set((v_array,(v_index,(newval,_))),st) =
  let idx = as_short(long_val(v_index)) in
  if (idx < 0) or (idx >= size_val(v_array)) then bound_error () else (
    set_field(v_array, idx, newval);
    (val_unit,st)
  ) ;;

let caml_array_unsafe_get((v_array,(v_index,_)),st) =
  let idx = as_short(long_val(v_index)) in
  let v = get_field(v_array, idx) in
  (v,st) ;;


let caml_array_unsafe_set((v_array,(v_index,(newval,_))),st) =
  let idx = as_short(long_val(v_index)) in
  set_field(v_array, idx, newval);
  (val_unit,st);;

let caml_array_get_addr(args,st) = caml_array_get(args,st) ;;
let caml_array_set_addr(args,st) = caml_array_set(args,st) ;;

let caml_array_unsafe_get_addr(args,st) = caml_array_get(args,st) ;;
let caml_array_unsafe_set_addr(args,st) = caml_array_set(args,st) ;;

