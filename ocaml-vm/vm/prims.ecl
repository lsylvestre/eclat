(* *********** alloc *************** *)

let rec pause_make_block args = make_block args ;;

let caml_make_block((tag,size),st) =
  let (pc,acc,sp, (env, extra_args, trap_sp), others) = st in
  let (acc,env,blk) = pause_make_block(sp,acc,env,tag,size) in
  let st = (pc,acc,sp, (env, extra_args, trap_sp), others) in
  (blk,st) ;;


(*let string_length((v,_),st) = (val_long(as_long(size_val(v))),st) ;;
let bytes_length = string_length ;;
*)

let alloc_block(tag,size,st) =
  let (pc,acc,sp, (env, extra_args, trap_sp), others) = st in
  let (acc,env,blk) = pause_make_block(sp,acc,env,tag,size) in
  let st = (pc,acc,sp, (env, extra_args, trap_sp), others) in
  (blk,st) ;;

let caml_cons ((v,(l,_)),st) =
  let (l2,st) = alloc_block(1,2,st) in
  set_field(l2,0,v);
  set_field(l2,1,l);
  (l2,st) ;;

(* ************** identity ************** *)

let caml_identity((v,_),st) = (v,st) ;;

(* *********** displaying *************** *)

let caml_print_int ((v1,_),st) =
  print_string "======> ";
  print_int (long_val v1);
  print_newline ();
  (val_unit,st) ;;

let caml_print_newline (_,st) =
  print_newline ();
  (val_unit,st) ;;

let caml_print_string ((v,_),st) = (* todo *)
  let sz = size_val(v) in
  let rec w(i) =
    if i = sz then () else
    (print_string "<char>(";
     print_int (long_val(get_field(v,i)));
     print_string ")";
     print_newline (); w(i+1))
  in
  w(0);
  (val_unit,st) ;;

(* *********** controlling a LED *************** *)

let caml_led_off (v,(pc,acc,sp,regs,(finished,_))) =
  (val_unit,(pc,acc,sp,regs,(finished,true))) ;;

let caml_led_on (v,(pc,acc,sp,regs,(finished,_))) =
  (val_unit,(pc,acc,sp,regs,(finished,false))) ;;

(* *********** comparisons *************** *)

let rec caml_compare ((v1,(v2,_)),st) = (* todo *)
  print_string "caml_compare:todo";
  let rec f () = f() in f ();
  (val_long 0,st);;

let caml_string_equal ((v1,(v2,_)),st) = (* todo *)
  (val_long 0,st);;


(*
let caml_compare ((v1,(v2,_)),st) =
  let x = v1 < v2 in
  let y = v1 = v2 in
  if x then -1 else
  if not(y) then 1 else
  if is_ptr(v1) && is_ptr(v2) then
    tag_val v1

  if is_long(v1) then
    if is_long(v2) & (long_val(v1) = long_val(v2))
  else
    (assert (is_ptr(v1));
     if is_ptr(v2) then
       let sz1 = size_val(v1) in
       let sz2 = size_val(v2) in
       if sz1 = sz2 then

       else sz2
  let b = long_val(v1) = long_val(v2) in
  let v = val_long(int_of_bool(b)) in
  (v,st) ;;
*)

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
  let c = caml_fresh.(0) in
  caml_fresh.(0) <- c + 1; 
  (val_long c,st) ;;

let caml_obj_dup((arg,_),st) =
  let sz = size_val(arg) in
  if sz == 0 then (arg,st) else (
  let tag = char_of_short(tag_val arg) in
  
  let (pc,acc,sp, (env, extra_args, trap_sp), others) = st in
  
  let (acc,env,blk) = pause_make_block(sp,acc,env,tag,sz) in
  
  let st = (pc,acc,sp, (env, extra_args, trap_sp), others) in
  
  let rec w(i) =
    if i >= sz then () else
    (set_field(blk,i,get_field(arg,i)); w(i+1))
  in
  w(0);
  (blk,st)) ;;


(******************** arrays ********************)

let bound_error (idx,n) =
  print_int idx; 
  print_string "/";
  print_int n;
  fatal_error "index out of bounds:" ;;

let caml_make_vect ((v_size,(v_init,_)),st) =
  let sz = as_short(long_val(v_size)) in

  let (pc,acc,sp, (env, extra_args, trap_sp), others) = st in

  let (acc,env,blk) = pause_make_block(sp,acc,env,0,sz) in

  let st = (pc,acc,sp, (env, extra_args, trap_sp), others) in
  let rec w(i) =
    if i = sz then () else
    (set_field(blk,i,v_init); w(i+1))
  in
  w(0);
  (blk,st) ;;


let rec check_bounds(idx,sz) =
  pause (if (idx < 0) or (idx >= sz) then bound_error (idx,sz) else ()) ;;

let caml_array_get((v_array,(v_index,_)),st) =
  let idx = as_short(long_val(v_index)) in
  let sz = size_val(v_array) in
  check_bounds(idx,sz);
  let v = get_field(v_array, idx) in
  (v,st) ;;


let caml_array_set((v_array,(v_index,(newval,_))),st) =
  let idx = as_short(long_val(v_index)) in
  let sz = size_val(v_array) in
  check_bounds(idx,sz);
  set_field(v_array, idx, newval);
  (val_unit,st) ;;

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


(* buggy: after make_block, if a GC has occur, 
   the pointer src may be out of the heap! 
*)
let caml_array_sub ((src,(ofs_v,(len_v,_))),st) = 
  (* todo: should check ofs/length *)
  let ofs = as_short(long_val(ofs_v)) in
  let len = as_short(long_val(len_v)) in
  print_string "FOO!![";
  print_int len;
  print_string "]";
  print_block src;
  print_string "~~~~";
  let (pc,acc,sp, (env, extra_args, trap_sp), others) = st in
  let (acc,env,blk) = pause_make_block(sp,acc,env,0,len) in
  let st = (pc,acc,sp, (env, extra_args, trap_sp), others) in
  let rec copy(i) =
    (* ************************** *)
    print_int i; print_string ";";
    (* ************************** *)
    if i >= len then (print_string "~~~~[";print_block blk; print_string "]"; (blk,st)) else
    (let idx = ofs+i in
     set_field(blk,idx,get_field(src,idx)); 
     copy(i+1))
  in copy(0) ;;


(******************** char ********************)

(* conversion from int to char with no bound verification.
   assume the argument is an integer value between 0 and 255. *)
let unsafe_chr = caml_identity ;;

(* create a bytes of size [v_size] initialized with zeros *)
let bytes_create ((v_size,_),st) =
  let sz = as_short(long_val(v_size)) in
  let (pc,acc,sp, (env, extra_args, trap_sp), others) = st in

  let (acc,env,blk) = pause_make_block(sp,acc,env,0,sz) in

  let st = (pc,acc,sp, (env, extra_args, trap_sp), others) in
  let rec w(i) =
    if i = sz then () else
    (set_field(blk,i,val_long 0); w(i+1))
  in
  w(0);
  (blk,st) ;;

let  caml_create_bytes args = bytes_create args ;;

let caml_ml_string_length((v,_),st) = (val_long(as_long(size_val(v))),st) ;;

let caml_blit_string((s1,(ofs1,(s2,(ofs2,n)))),st) =
  let ofs1 = as_short(long_val(ofs1)) in
  let ofs2 = as_short(long_val(ofs2)) in
  let n = as_short(long_val(n)) in
  let rec copy(i) = (* todo: should precisely implement the C function memmove *)
    if i > n then (val_unit,st) else (* ok ? *)
    (set_field(s2,ofs2+i,get_field(s1,ofs1+i)); copy(i+1))
  in copy(0) ;;

let caml_string_of_bytes = caml_identity ;; (* ok ? *)


let caml_string_get (args,st) = (val_unit,st);; (* todo *)

(* bytes physical modification, with no bound verification *)
let bytes_unsafe_set((v_bytes,(v_index,(newval,_))),st) =
  let idx = as_short(long_val(v_index)) in
  set_field(v_bytes, idx, newval);
  (val_unit,st);;

(* convert bytes to string, NB: no copy *)
let unsafe_to_string = caml_identity ;;