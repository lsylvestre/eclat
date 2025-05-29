(* CODE GENERATED FROM ECLAT SOURCE PROGRAM *)

(** custom word/half/char sizes *)

type long = int ;;
type short = int ;;
type char = int ;;

let as_long (n:short) : long = (n) ;;

let short_of_char (n:char) : short = (n) ;;
let as_short (n:long) : short = (n) land (1 lsl 16 - 1) ;;

let char_of_long (n:long) : char = (n) ;;
let long_of_char (n:char) : long = (n) ;;
let char_of_short (n:short) : char = (n) land (1 lsl 8 - 1) ;;
let short_of_char (n:char) : short = (n) ;;
let stack = Array.make 4096 (0,false) ;;
let ram = Array.make 32768 (0,false) ;;

let global_end = Array.make 1 0 ;;

let from_space_array = Array.make 1 0 ;;
let to_space_array = Array.make 1 0 ;;
let next_array = Array.make 1 0 ;;



let caml_fresh = Array.make 1 0 ;;


let pause a = a ;;

let length = Array.length ;;

(** representation of values *)

type is_int = bool ;;

type value = long * is_int ;;


let int_of_bool b =
  if b then 1 else 0 ;;

(* constants *)
let val_unit : value = (1,true) ;;
let val_true : value = (1,false) ;;
let val_false : value = (0,false) ;;

(* conversions *)

let long_val (n,_) = n ;;
let ptr_val (n,_) = as_short(n) ;;

let is_int (_,b) = b ;;
let is_long = is_int ;;
let is_ptr (_,b) = not b ;;

let val_int (n:long) : value = (n,true) ;;
let val_long n = val_int(n);; (* synonym *)

let val_ptr (n:short) : value = (as_long(n),false) ;;



let no_scan_tag : char = 251 ;;
let string_tag : char = 252 ;;
let closure_tag : char = 247 ;;
let infix_tag : char = 249 ;;
let fwd_ptr_tag : char = 248 ;;


let make_header ((tag,sz) : (char * short)) : long =
  (long_of_char(tag) lsl 24) lor (as_long(sz) lsl 2) ;;
type unop = NOT of unit 
          | NEG of unit
          | VECTLENGTH of unit
          | ISINT of unit

type compare_op = LT of unit   | GT of unit   | EQ  of unit  | NEQ  of unit  | LE of unit   | GE of unit  

type group1 =  
              ACC           of unit
            | PUSH          of unit
            | POP           of unit
            | ASSIGN        of unit
            | MAKEFLOATBLOCK of long (* todo*)
            | ENVACC        of unit
            | CONST         of long            
            | OFFSET        of long
            | OFFSETREF     of long
            | UNOP          of unop
            | GETMETHOD     of unit
            | GETPUBMET     of unit
            | GETDYNMET     of unit
            | GETFIELD      of unit
            | SETFLOATFIELD of unit (* todo*)
            | BUCOMPARE     of long * bool
             (* BCOMPARE : todo *)
            | OFFSETCLOSURE of unit (* ptr *)
            | PUSHRETADDR   of unit
            | GETGLOBAL     of unit
            | SETGLOBAL     of unit
             ;;
type group2 = 
              GETVECTITEM   of unit
            | SETVECTITEM   of unit
            | GETSTRINGCHAR of unit
            | SETSTRINGCHAR of unit
            | ADD           of unit
            | SUB           of unit
            | MUL           of unit
            | OR            of unit
            | AND           of unit
            | MOD           of unit
            | DIV           of unit
            | LSL           of unit
            | ASR           of unit
            | XOR           of unit
            | COMPARE       of compare_op
            | SETFIELD      of short

type group3 =
             BRANCH         of short
            | BRANCHIF      of bool * short
            | BCOMPARE      of compare_op * long * short
            | STOP          of unit
            | SWITCH        of short * short
            | CHECK_SIGNALS of unit

type group4 =
   POPTRAP       of unit
             | RAISE         of unit
            | RESTART       of unit 


type opcode =
    LABEL         of short
  | GROUP1        of group1 * short
  | GROUP2        of group2 
  | GROUP3        of group3 
  | GROUP4        of group4
  | MAKEBLOCK     of bool * bool * short * char * short(* is_atom * size * tag *)
  | RETURN        of short
  | APPTERM       of short * short
  | APPLY         of bool * char * short
  | PUSHTRAP      of short
  | CLOSURE       of short * short
  | GRAB          of char
  | CALL          of short * bool * bool * bool * bool
  | CLOSUREREC    of char * short * short * short       (* char: up to 256 mutual recursive values *)                  
let fatal_error msg =
  (* fatal error (implemented as an infinite loop)
     in case of invalid_arg error.

     note: cannot be catched by the OCaml program *)
 
  print_string "fatal error: ";
  print_string msg;
  print_newline ();

  let rec forever () = 
    forever ()
  in 
  forever () ;;

let stack_start : short = 1;;
let heap_start : short = 4096 ;;
let heap_size : short = 8192 ;;

let global_start : short = 0 ;;

(* the end of the section containing the OCaml global,
   global_end[0] is set during loading the bytecode *)

(* [rec] annotation introduces a pause, augmenting the throughput, but potentially
   breaking the critical path and diminishing the area of the generated hardware *)
let (*rec*) ram_get (i) = ram.(i) ;;
let (*rec*) ram_set (i,v) = ram.(i) <- v ;;

let global_get n =
  ram.(global_start + n) ;;

let global_set ((n,v) : (short * value)) =
  ram.(global_start + n) <- v ;;


let stack_get i = stack.(i) ;;
let stack_set (i,v) = stack.(i) <- v ;;
 

let push_stack ((v,sp) : (value * short)) : short =
  let sp_plus_1 = sp + 1 in
  stack_set(sp, v);
  sp_plus_1 ;;

let pop_stack (sp) =
  (* assert (sp[0] > 0); *)
  let p = sp - 1 in
  let v = stack_get(p) in
  (v,p) ;;

let get_field(v,i) =
  ram_get(ptr_val(v) + i + 1) ;;

let set_field(v,i,w) =
  ram_set(ptr_val(v) + i + 1,  w) ;;

let get_field0(v) =
  ram_get(ptr_val(v) + 1) ;;

let get_field1(v) =
  ram_get(ptr_val(v) + 2) ;;

let set_field0(v,w) =
  ram_set(ptr_val(v) + 1,  w) ;;

let set_field1(v,w) =
  ram_set(ptr_val(v) + 2,  w) ;;

let get_stack_ofs(sp,i) =
  stack_get(sp - i - 1);;

let bnot n = int_of_bool (n == 0) ;;

let addint (n,m) = n + m ;;
let subint (n,m) = n - m ;;
let mulint (n,m) = n * m ;;

let bxor(a,b) =
  if a then (not b) else b ;;

let divint (n,m) =
  if m == 0 then 0 (* todo: raise error *) else
  let rec div (a,b,acc) =
    if a < b then acc else div(a-b, b, acc+1)
  in
  let r = div(abs n, abs m, 0) in
  if bxor (n >= 0,m >= 0) then 0 - r else r ;;

let modint (n,m) =
  if m == 0 then 0 (* todo: raise error *) else
  let rec modulo (a,b) =
    if a < b then a else modulo(a-b, b)
  in
  let r = modulo(abs n, abs m) in
  if (n < 0) then 0 - r else r ;;

let andint (n,m) = n land m ;;
let orint (n,m) = n lor m ;;
let xorint (n,m) = n lxor m ;;
let lslint (n,m) = n lsl m ;;
let lsrint (n,m) = n lsr m ;;
let asrint (n,m) = n asr m ;;

let eq (n,m) = int_of_bool (n == m) ;;
let neq (n,m) = int_of_bool (n <> m) ;;
let ltint (n,m) = int_of_bool (n < m) ;;
let leint (n,m) = int_of_bool (n <= m) ;;
let gtint (n,m) = int_of_bool (n > m) ;;
let geint (n,m) = int_of_bool (n >= m) ;;

let compare_imm(n1,n2) =
  if n1 < n2 then -1 else
  if n1 > n2 then 1 else 0 ;;


(* stack manipulation *)


let pop_stack_implace (sp_minus_1) =
  (* assert (sp[0] > 0); (see bug 3) *)
  let v = ram.(sp_minus_1) in
  v ;;

(*** unsigned comparison (<)                  ***)
(*** n1 < 0 && n2 >= 0 => (ultint n1 n2) ~> 0 ***)
let ultint (n1,n2) =
  if n1 < 0 then (if n2 < 0 then gtint(n1,n2) else 0)
  else (if n2 < 0 then 0 else ltint(n1,n2)) ;;


(*** unsigned comparison (>=)                 ***)
(*** n1 < 0 && n2 >= 0 => (ugeint n1 n2) ~> 1 ***)
let ugeint (n1,n2) =
  if n1 < 0 then (if n2 < 0 then leint(n1,n2) else 1)
  else (if n2 < 0 then 1 else geint(n1,n2)) ;;


let print_val (v:value) =
  print_int (long_val v);
  print_string "<";
  (if is_int(v) then print_string "int" else print_string "ptr");
  print_string ">" ;;


let print_stack(sp) =
  print_string "stack: [";
  let rec w(i) =
    if i >= sp then () else
    let v = ram.(i) in (
      print_int (long_val(v));
      print_string "|";
      w(i+1)
    )
  in
  w(stack_start);
  print_string "]";
  print_newline () ;;


let size_hd hd =
  (* [as_short] (i.e. resize) is used to forget higher bits (tag) *)
  as_short (hd lsr 2) ;;

let size (ptr:short) : short =
  let hd = ram.(ptr) in
  size_hd (as_long(ptr_val(hd))) ;;

let tag ptr =
  let hd = ram.(ptr) in
  (ptr_val(hd)) lsr 24 ;;

let size_val v = size (ptr_val v) ;;
let tag_val v : short = tag (ptr_val v) ;;

(* =========================== GC =========================== *)

let message_start_gc () =
  print_newline ();
  print_newline ();
  print_string "[================= GC START ======================]";
  print_newline ();
  print_newline () ;;

let message_gc_middle () =
print_string "=======================================";
print_newline () ;;


let message_end_gc () =
  print_newline ();
  print_newline ();
  print_string "[================= GC END ======================]";
  print_newline ();
  print_newline () ;;

(* [is_pointer(start_space,v)] returns [true] if value [v] is a pointer
   in the semi_space starting at [start_space] *)
let is_pointer(start_space,v) =
  is_ptr v && (let p = ptr_val v in
               pause(start_space <= p) && (p < start_space + heap_size)) ;;

(* [copy(from_space, to_space, v, next)] moves value [v]
   to the first available place [next] in [to_space] if [v] is a pointer in [from_space]. *)
let copy(from_space, to_space, v, next) =
  if not(is_pointer(from_space,v))
  then
    (v,next)
  else (
    let p = ptr_val v in
    let w = ram.(p + 1) in
    if is_pointer(to_space,w) then
      (* [v] has already been copied in [to_space] at address [ptr_of_val(w)] *)
      (w,next)
    else (
      (* [v] must be copied in [to_space] *)
      let hd = ram.(p) in
      let sz = size_hd (long_val(hd)) + 1 in

      print_string "bloc ";      print_int p ;
      print_string " of size ";  print_int sz;
      print_string " from ";     print_int (p);
      print_string " to ";       print_int next;  print_newline ();

      ram.(next) <- hd;

      let rec loop(i) =
        if i = sz (* >= *) then () else
        (ram.(next+i) <- ram.(p+i); loop(i+1))
      in
      loop(1);
      ram.(p + 1) <- val_ptr(next);
      (val_ptr(next),next + sz)
    )
  )
;;



let stop_and_copy (sp,acc,env,from_space,to_space) =

 message_start_gc ();

 let next = to_space in

  (* ================ starting with roots: acc, env, stack, globals ================ *)
  let (next_acc,next) = (copy (from_space, to_space, acc, next)) in
  let (next_env,next) = (copy (from_space, to_space, env, next)) in

  let rec copy_root_in_ram(i,section_end,next) =
    if i = section_end (* >= *) then next else
    (print_string "racine:"; print_int i; print_newline ();
    let (w,next) = (copy (from_space, to_space, ram.(i), next)) in
    ram.(i) <- w;
    print_string " next="; print_int next; print_newline ();
    copy_root_in_ram(i+1,section_end,next))
  in
  let next = copy_root_in_ram(global_start,global_end.(0),next) in  

  let rec copy_root_in_stack(i,section_end,next) =
    if i = section_end (* >= *) then next else
    (print_string "racine:"; print_int i; print_newline ();
    let (w,next) = (copy (from_space, to_space, stack.(i), next)) in
    stack.(i) <- w;
    print_string " next="; print_int next; print_newline ();
    copy_root_in_stack(i+1,section_end,next))
  in
  let next = copy_root_in_stack(stack_start,sp,next) in

  message_gc_middle ();

  (* scan objects in to_space (including objects added by this loop) *)
  let rec aux(scan,next) =

    print_string "     scan="; print_int scan ;  print_string " | next="; print_int next; print_newline ();

    if scan >= next then next else
    let sz = size_hd (as_long (ptr_val (ram.(scan)))) + 1 in
    let rec loop(i,next) =
      (* print_string " ~~ scan"; print_newline (); *)
      if i = sz (* >= *) then next else
      let scan_plus_i = scan + i in
      let (w,next) = (copy (from_space, to_space, ram.(scan_plus_i),next)) in
      ram.(scan_plus_i) <- w;
      loop(i+1,next)
    in
    let next = loop(1,next) in
    aux(scan + sz,next)
  in
  let next = aux (to_space,next) in

  let mem_copied = next - to_space in
  print_string "memory copied in to_space : ";
  print_int mem_copied;
  print_string " words";
  print_newline ();

  (next_acc,next_env,next)
;;

let gc(sp,acc,env,from_space, to_space) =
  stop_and_copy (sp,acc,env,from_space,to_space) ;;

(* =========================== ALLOC =========================== *)

let gc_alloc ((sp, acc, env,sz) : (short * value *value * short)) : (value * value * short) =
  print_string "GC-ALLOC:(size="; print_int sz; print_string ")"; print_newline ();
  
  let from_space = from_space_array.(0) in
  let to_space = to_space_array.(0) in
  let next = next_array.(0) in
  let next_plus_sz = next + sz in
  let from_space_plus_heap_size = from_space + heap_size in
      (* print_string "~~~~~~> "; print_int next_plus_sz;
         print_string "|~~~~~~> "; print_int from_space_plus_heap_size; print_newline (); *)
  if pause (next_plus_sz > from_space_plus_heap_size)
  then (
    let (next_acc,next_env,next) = gc(sp,acc,env,from_space, to_space) in
    let mem_copied = next - to_space in
    if mem_copied + sz > heap_size then fatal_error "Out of memory";

    message_end_gc ();

(* 
  print_string "acc:"; print_val next_acc;
  print_string "env:"; print_val next_env;
  print_stack(sp); print_newline ();
  print_string "heap";
  let rec loop(i) =
  if i >= heap_size then () else
  ( print_string "["; print_int (to_space+i);print_string "]";
    print_val(ram[to_space+i]);print_string ";";
  loop(i+1) )
  in loop(0); *)
     next_array.(0) <- next + sz;
     from_space_array.(0) <- to_space;
     to_space_array.(0) <- from_space;
    (next_acc,next_env,next))
  else (
    next_array.(0) <- next_plus_sz;
     (acc,env,next)
  ) ;;



(* =========================== MAKE_BLOCK =========================== *)

let rec make_block(sp, acc, env, tag,sz) : (value * value * value) =
  let sz = if sz = 0 then 1 else sz in
  let (next_acc,next_env, a) = gc_alloc (sp, acc, env,sz + 1) in
   print_string "size:"; print_int(sz); print_newline();
  let hd = (make_header(tag,sz)) in

  ram.(a) <- val_int(hd);
  (* print_string "make_block:"; print_int(size_hd hd); print_newline();
  print_string "HEADER:"; print_int a; print_string "  "; print_int (long_val(ram[a])); print_newline (); *)
  (next_acc,next_env,val_ptr(a)) ;;

(*
let make_closure ((sp,acc,env, pc,size) : (short * value * value * short * short)) : (value * value * value) =
  let (next_acc,next_env,res) = make_block(sp, acc,env, closure_tag,size) in
  set_field(res,0,val_int (as_long(pc)));
  (next_acc,next_env,res) ;;
*)





let print_block(v) =
  let a = ptr_val v in
  let n = size a in
  print_string "{size:"; print_int n;
  print_string "}[";
  for i = 0 to (n - 1) do
    let x = ram.(a+i+1) in
    print_val x;
    print_string ";"
  done;
  print_string "]";;
 


let rec pause_make_block args = make_block args ;;

(*let string_length((v,_),st) = (val_long(as_long(size_val(v))),st) ;;
let bytes_length = string_length ;;
*)
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

let caml_compare ((v1,(v2,_)),st) = (* todo *)
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

(* let caml_array_sub (v,s) =*)

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
  let ofs1 = long_val(ofs1) in
  let ofs2 = long_val(ofs2) in
  let n = long_val(n) in
  let rec copy(i) = (* todo: should precisely implement the C function memmove *)
    if i > n then (val_unit,st) else (* ok ? *)
    (set_field(s2,ofs2+i,get_field(s1,ofs1+i)); copy(i+1))
  in copy(0) ;;

let caml_string_of_bytes = caml_identity ;; (* ok ? *)


(* bytes physical modification, with no bound verification *)
let bytes_unsafe_set((v_bytes,(v_index,(newval,_))),st) =
  let idx = as_short(long_val(v_index)) in
  set_field(v_bytes, idx, newval);
  (val_unit,st);;

(* convert bytes to string, NB: no copy *)
let unsafe_to_string = caml_identity ;;(* THIS FILE HAS BEEN GENERATED *)

let external_call (n,args,env) =
  match n with
  | 0 -> caml_identity(args,env)
  | 1 -> caml_print_string(args,env)
  | 2 -> caml_print_int(args,env)
  | 3 -> caml_led_off(args,env)
  | 4 -> caml_led_on(args,env)
  | 5 -> caml_equal(args,env)
  | 6 -> caml_lessthan(args,env)
  | 7 -> caml_greaterthan(args,env)
  | 8 -> caml_greaterequal(args,env)
  | 9 -> caml_lessequal(args,env)
  | 10 -> caml_fresh_oo_id(args,env)
  | 11 -> caml_make_vect(args,env)
  | 12 -> caml_obj_dup(args,env)
  | 13 -> bytes_create(args,env)
  | 14 -> caml_array_get(args,env)
  | 15 -> caml_array_set(args,env)
  | 16 -> caml_array_unsafe_set(args,env)
  | 17 -> caml_array_unsafe_get(args,env)
  | 18 -> caml_array_get_addr(args,env)
  | 19 -> caml_array_set_addr(args,env)
  | 20 -> caml_array_unsafe_get_addr(args,env)
  | 21 -> caml_array_unsafe_set_addr(args,env)
  | 22 -> unsafe_chr(args,env)
  | 23 -> bytes_unsafe_set(args,env)
  | 24 -> unsafe_to_string(args,env)
  | 25 -> caml_ml_string_length(args,env)
  | 26 -> caml_create_bytes(args,env)
  | 27 -> caml_blit_string(args,env)
  | 28 -> caml_string_of_bytes(args,env)
  | 29 -> caml_compare(args,env)
  | 30 -> caml_array_sub(args,env)
  | 31 -> caml_string_get(args,env)
  | 32 -> caml_string_equal(args,env)
  | 33 -> caml_print_newline(args,env)
  | _ -> print_string "unknown primitive"; (val_unit,env)
  ;;


let code = Array.make 4096 (GROUP3(STOP()):opcode) ;;
let data_rom = Array.make 2048 (0,false) ;;


 let load_code () =
  (code.(0) <- GROUP3(BRANCH(70)));
  (code.(1) <- GROUP4(RESTART()));
  (code.(2) <- GRAB(1));
  (code.(3) <- GROUP1(ACC(),0));
  (code.(4) <- (CALL(25,false,false,false,false)));
  (code.(5) <- GROUP1(PUSH(),0));
  (code.(6) <- GROUP1(ACC(),2));
  (code.(7) <- (CALL(25,false,false,false,false)));
  (code.(8) <- GROUP1(PUSH(),0));
  (code.(9) <- GROUP1(ACC(),0));
  (code.(10) <- GROUP1(PUSH(),0));
  (code.(11) <- GROUP1(ACC(),2));
  (code.(12) <- GROUP2(ADD()));
  (code.(13) <- (CALL(26,false,false,false,false)));
  (code.(14) <- GROUP1(PUSH(),0));
  (code.(15) <- GROUP1(ACC(),2));
  (code.(16) <- GROUP1(PUSH(),0));
  (code.(17) <- GROUP1(CONST(0),0));
  (code.(18) <- GROUP1(PUSH(),0));
  (code.(19) <- GROUP1(ACC(),2));
  (code.(20) <- GROUP1(PUSH(),0));
  (code.(21) <- GROUP1(CONST(0),0));
  (code.(22) <- GROUP1(PUSH(),0));
  (code.(23) <- GROUP1(ACC(),7));
  (code.(24) <- (CALL(27,true,true,true,false)));
  (code.(25) <- GROUP1(ACC(),1));
  (code.(26) <- GROUP1(PUSH(),0));
  (code.(27) <- GROUP1(ACC(),3));
  (code.(28) <- GROUP1(PUSH(),0));
  (code.(29) <- GROUP1(ACC(),2));
  (code.(30) <- GROUP1(PUSH(),0));
  (code.(31) <- GROUP1(CONST(0),0));
  (code.(32) <- GROUP1(PUSH(),0));
  (code.(33) <- GROUP1(ACC(),8));
  (code.(34) <- (CALL(27,true,true,true,false)));
  (code.(35) <- GROUP1(ACC(),0));
  (code.(36) <- (CALL(28,false,false,false,false)));
  (code.(37) <- RETURN(5));
  (code.(38) <- GROUP1(ACC(),0));
  (code.(39) <- GROUP1(PUSH(),0));
  (code.(40) <- GROUP1(GETGLOBAL(),3));
  (code.(41) <- MAKEBLOCK(false,false,2,0,0));
  (code.(42) <- GROUP4(RAISE()));
  (code.(43) <- GROUP1(ACC(),0));
  (code.(44) <- GROUP1(PUSH(),0));
  (code.(45) <- GROUP1(GETGLOBAL(),2));
  (code.(46) <- MAKEBLOCK(false,false,2,0,0));
  (code.(47) <- GROUP4(RAISE()));
  (code.(48) <- GROUP4(RESTART()));
  (code.(49) <- GRAB(1));
  (code.(50) <- GROUP1(ACC(),1));
  (code.(51) <- GROUP1(PUSH(),0));
  (code.(52) <- GROUP1(ACC(),1));
  (code.(53) <- (CALL(8,true,false,false,false)));
  (code.(54) <- GROUP3(BRANCHIF(false,57)));
  (code.(55) <- GROUP1(ACC(),0));
  (code.(56) <- RETURN(2));
  (code.(57) <- GROUP1(ACC(),1));
  (code.(58) <- RETURN(2));
  (code.(59) <- GROUP4(RESTART()));
  (code.(60) <- GRAB(1));
  (code.(61) <- GROUP1(ACC(),1));
  (code.(62) <- GROUP1(PUSH(),0));
  (code.(63) <- GROUP1(ACC(),1));
  (code.(64) <- (CALL(9,true,false,false,false)));
  (code.(65) <- GROUP3(BRANCHIF(false,68)));
  (code.(66) <- GROUP1(ACC(),0));
  (code.(67) <- RETURN(2));
  (code.(68) <- GROUP1(ACC(),1));
  (code.(69) <- RETURN(2));
  (code.(70) <- MAKEBLOCK(false,true,0,closure_tag,60));
  (code.(71) <- GROUP1(SETGLOBAL(),76));
  (code.(72) <- MAKEBLOCK(false,true,0,closure_tag,49));
  (code.(73) <- GROUP1(SETGLOBAL(),75));
  (code.(74) <- MAKEBLOCK(false,true,0,closure_tag,43));
  (code.(75) <- GROUP1(SETGLOBAL(),14));
  (code.(76) <- MAKEBLOCK(false,true,0,closure_tag,38));
  (code.(77) <- GROUP1(SETGLOBAL(),16));
  (code.(78) <- MAKEBLOCK(false,true,0,closure_tag,2));
  (code.(79) <- GROUP1(SETGLOBAL(),24));
  (code.(80) <- GROUP3(BRANCH(211)));
  (code.(81) <- GROUP4(RESTART()));
  (code.(82) <- GRAB(1));
  (code.(83) <- GROUP1(ACC(),1));
  (code.(84) <- GROUP3(BRANCHIF(false,93)));
  (code.(85) <- GROUP1(ACC(),1));
  (code.(86) <- GROUP1(GETFIELD(),1));
  (code.(87) <- GROUP1(PUSH(),0));
  (code.(88) <- GROUP1(ACC(),1));
  (code.(89) <- GROUP1(OFFSET(1),0));
  (code.(90) <- GROUP1(PUSH(),0));
  (code.(91) <- GROUP1(OFFSETCLOSURE(),0));
  (code.(92) <- APPTERM(2,4));
  (code.(93) <- GROUP1(ACC(),0));
  (code.(94) <- RETURN(2));
  (code.(95) <- GROUP4(RESTART()));
  (code.(96) <- GRAB(1));
  (code.(97) <- GROUP1(ACC(),0));
  (code.(98) <- GROUP3(BRANCHIF(false,110)));
  (code.(99) <- GROUP1(ACC(),1));
  (code.(100) <- GROUP1(PUSH(),0));
  (code.(101) <- GROUP1(ACC(),1));
  (code.(102) <- GROUP1(GETFIELD(),0));
  (code.(103) <- MAKEBLOCK(false,false,2,0,0));
  (code.(104) <- GROUP1(PUSH(),0));
  (code.(105) <- GROUP1(ACC(),1));
  (code.(106) <- GROUP1(GETFIELD(),1));
  (code.(107) <- GROUP1(PUSH(),0));
  (code.(108) <- GROUP1(OFFSETCLOSURE(),0));
  (code.(109) <- APPTERM(2,4));
  (code.(110) <- GROUP1(ACC(),1));
  (code.(111) <- RETURN(2));
  (code.(112) <- GROUP4(RESTART()));
  (code.(113) <- GRAB(2));
  (code.(114) <- GROUP1(ACC(),2));
  (code.(115) <- GROUP3(BRANCHIF(false,131)));
  (code.(116) <- GROUP1(ACC(),2));
  (code.(117) <- GROUP1(GETFIELD(),1));
  (code.(118) <- GROUP1(PUSH(),0));
  (code.(119) <- GROUP1(ACC(),3));
  (code.(120) <- GROUP1(GETFIELD(),0));
  (code.(121) <- GROUP1(PUSH(),0));
  (code.(122) <- GROUP1(ACC(),3));
  (code.(123) <- GROUP1(PUSH(),0));
  (code.(124) <- GROUP1(ACC(),3));
  (code.(125) <- APPLY(true,2,0));
  (code.(126) <- GROUP1(PUSH(),0));
  (code.(127) <- GROUP1(ACC(),2));
  (code.(128) <- GROUP1(PUSH(),0));
  (code.(129) <- GROUP1(OFFSETCLOSURE(),0));
  (code.(130) <- APPTERM(3,6));
  (code.(131) <- GROUP1(ACC(),1));
  (code.(132) <- RETURN(3));
  (code.(133) <- GROUP4(RESTART()));
  (code.(134) <- GRAB(1));
  (code.(135) <- GROUP1(ACC(),1));
  (code.(136) <- GROUP3(BRANCHIF(false,153)));
  (code.(137) <- GROUP1(ACC(),0));
  (code.(138) <- GROUP1(PUSH(),0));
  (code.(139) <- GROUP1(ACC(),2));
  (code.(140) <- GROUP1(GETFIELD(),0));
  (code.(141) <- (CALL(29,true,false,false,false)));
  (code.(142) <- GROUP1(PUSH(),0));
  (code.(143) <- GROUP1(CONST(0),0));
  (code.(144) <- GROUP2(COMPARE(EQ())));
  (code.(145) <- GROUP3(BRANCHIF(true,153)));
  (code.(146) <- GROUP1(ACC(),1));
  (code.(147) <- GROUP1(GETFIELD(),1));
  (code.(148) <- GROUP1(PUSH(),0));
  (code.(149) <- GROUP1(ACC(),1));
  (code.(150) <- GROUP1(PUSH(),0));
  (code.(151) <- GROUP1(OFFSETCLOSURE(),0));
  (code.(152) <- APPTERM(2,4));
  (code.(153) <- RETURN(2));
  (code.(154) <- GROUP1(CONST(0),0));
  (code.(155) <- GROUP1(PUSH(),0));
  (code.(156) <- GROUP1(ACC(),1));
  (code.(157) <- GROUP1(PUSH(),0));
  (code.(158) <- GROUP1(GETGLOBAL(),12));
  (code.(159) <- APPTERM(2,3));
  (code.(160) <- GROUP4(RESTART()));
  (code.(161) <- GRAB(1));
  (code.(162) <- GROUP1(ACC(),0));
  (code.(163) <- GROUP3(BRANCHIF(false,177)));
  (code.(164) <- GROUP1(ACC(),1));
  (code.(165) <- GROUP3(BCOMPARE(NEQ(),0,169)));
  (code.(166) <- GROUP1(ACC(),0));
  (code.(167) <- GROUP1(GETFIELD(),0));
  (code.(168) <- RETURN(2));
  (code.(169) <- GROUP1(ACC(),1));
  (code.(170) <- GROUP1(OFFSET(-1),0));
  (code.(171) <- GROUP1(PUSH(),0));
  (code.(172) <- GROUP1(ACC(),1));
  (code.(173) <- GROUP1(GETFIELD(),1));
  (code.(174) <- GROUP1(PUSH(),0));
  (code.(175) <- GROUP1(OFFSETCLOSURE(),0));
  (code.(176) <- APPTERM(2,4));
  (code.(177) <- GROUP1(GETGLOBAL(),13));
  (code.(178) <- GROUP1(PUSH(),0));
  (code.(179) <- GROUP1(GETGLOBAL(),14));
  (code.(180) <- APPTERM(1,3));
  (code.(181) <- GROUP4(RESTART()));
  (code.(182) <- GRAB(1));
  (code.(183) <- GROUP1(ACC(),1));
  (code.(184) <- GROUP3(BCOMPARE(LE(),0,189)));
  (code.(185) <- GROUP1(GETGLOBAL(),15));
  (code.(186) <- GROUP1(PUSH(),0));
  (code.(187) <- GROUP1(GETGLOBAL(),16));
  (code.(188) <- APPTERM(1,3));
  (code.(189) <- CLOSUREREC(1,0,2561,161));
  (code.(190) <- GROUP1(ACC(),2));
  (code.(191) <- GROUP1(PUSH(),0));
  (code.(192) <- GROUP1(ACC(),2));
  (code.(193) <- GROUP1(PUSH(),0));
  (code.(194) <- GROUP1(ACC(),2));
  (code.(195) <- APPTERM(2,5));
  (code.(196) <- GROUP1(ACC(),0));
  (code.(197) <- GROUP3(BRANCHIF(false,201)));
  (code.(198) <- GROUP1(ACC(),0));
  (code.(199) <- GROUP1(GETFIELD(),0));
  (code.(200) <- RETURN(1));
  (code.(201) <- GROUP1(GETGLOBAL(),17));
  (code.(202) <- GROUP1(PUSH(),0));
  (code.(203) <- GROUP1(GETGLOBAL(),14));
  (code.(204) <- APPTERM(1,2));
  (code.(205) <- GROUP1(ACC(),0));
  (code.(206) <- GROUP1(PUSH(),0));
  (code.(207) <- GROUP1(CONST(0),0));
  (code.(208) <- GROUP1(PUSH(),0));
  (code.(209) <- GROUP1(GETGLOBAL(),18));
  (code.(210) <- APPTERM(2,3));
  (code.(211) <- CLOSUREREC(1,0,2562,82));
  (code.(212) <- GROUP1(ACC(),0));
  (code.(213) <- GROUP1(SETGLOBAL(),18));
  (code.(214) <- MAKEBLOCK(false,true,0,closure_tag,205));
  (code.(215) <- GROUP1(SETGLOBAL(),73));
  (code.(216) <- MAKEBLOCK(false,true,0,closure_tag,196));
  (code.(217) <- GROUP1(SETGLOBAL(),33));
  (code.(218) <- MAKEBLOCK(false,true,0,closure_tag,182));
  (code.(219) <- GROUP1(SETGLOBAL(),74));
  (code.(220) <- CLOSUREREC(1,0,2563,96));
  (code.(221) <- GROUP1(ACC(),0));
  (code.(222) <- GROUP1(SETGLOBAL(),12));
  (code.(223) <- MAKEBLOCK(false,true,0,closure_tag,154));
  (code.(224) <- GROUP1(SETGLOBAL(),77));
  (code.(225) <- CLOSUREREC(1,0,2564,113));
  (code.(226) <- GROUP1(ACC(),0));
  (code.(227) <- GROUP1(SETGLOBAL(),70));
  (code.(228) <- CLOSUREREC(1,0,2565,134));
  (code.(229) <- GROUP1(ACC(),0));
  (code.(230) <- GROUP1(SETGLOBAL(),30));
  (code.(231) <- GROUP1(POP(),2));
  (code.(232) <- GROUP3(BRANCH(410)));
  (code.(233) <- GROUP4(RESTART()));
  (code.(234) <- GRAB(1));
  (code.(235) <- GROUP1(ACC(),1));
  (code.(236) <- GROUP3(BRANCHIF(false,245)));
  (code.(237) <- GROUP1(ACC(),1));
  (code.(238) <- GROUP1(GETFIELD(),1));
  (code.(239) <- GROUP1(PUSH(),0));
  (code.(240) <- GROUP1(ACC(),1));
  (code.(241) <- GROUP1(OFFSET(1),0));
  (code.(242) <- GROUP1(PUSH(),0));
  (code.(243) <- GROUP1(OFFSETCLOSURE(),0));
  (code.(244) <- APPTERM(2,4));
  (code.(245) <- GROUP1(ACC(),0));
  (code.(246) <- RETURN(2));
  (code.(247) <- GROUP4(RESTART()));
  (code.(248) <- GRAB(1));
  (code.(249) <- GROUP1(ACC(),1));
  (code.(250) <- GROUP3(BRANCHIF(false,266)));
  (code.(251) <- GROUP1(ACC(),1));
  (code.(252) <- GROUP1(GETFIELD(),0));
  (code.(253) <- GROUP1(PUSH(),0));
  (code.(254) <- GROUP1(ACC(),1));
  (code.(255) <- GROUP1(PUSH(),0));
  (code.(256) <- GROUP1(ENVACC(),1));
  (code.(257) <- (CALL(16,true,true,false,false)));
  (code.(258) <- GROUP1(ACC(),1));
  (code.(259) <- GROUP1(GETFIELD(),1));
  (code.(260) <- GROUP1(PUSH(),0));
  (code.(261) <- GROUP1(ACC(),1));
  (code.(262) <- GROUP1(OFFSET(1),0));
  (code.(263) <- GROUP1(PUSH(),0));
  (code.(264) <- GROUP1(OFFSETCLOSURE(),0));
  (code.(265) <- APPTERM(2,4));
  (code.(266) <- GROUP1(ENVACC(),1));
  (code.(267) <- RETURN(2));
  (code.(268) <- GROUP1(ACC(),0));
  (code.(269) <- GROUP3(BRANCHIF(false,290)));
  (code.(270) <- GROUP1(ACC(),0));
  (code.(271) <- GROUP1(GETFIELD(),0));
  (code.(272) <- GROUP1(PUSH(),0));
  (code.(273) <- GROUP1(ACC(),1));
  (code.(274) <- GROUP1(PUSH(),0));
  (code.(275) <- GROUP1(CONST(0),0));
  (code.(276) <- GROUP1(PUSH(),0));
  (code.(277) <- GROUP1(GETGLOBAL(),19));
  (code.(278) <- APPLY(true,2,0));
  (code.(279) <- (CALL(11,true,false,false,false)));
  (code.(280) <- GROUP1(PUSH(),0));
  (code.(281) <- GROUP1(ACC(),0));
  (code.(282) <- CLOSUREREC(1,1,2566,248));
  (code.(283) <- GROUP1(ACC(),2));
  (code.(284) <- GROUP1(GETFIELD(),1));
  (code.(285) <- GROUP1(PUSH(),0));
  (code.(286) <- GROUP1(CONST(1),0));
  (code.(287) <- GROUP1(PUSH(),0));
  (code.(288) <- GROUP1(ACC(),2));
  (code.(289) <- APPTERM(2,5));
  (code.(290) <- MAKEBLOCK(true,false,1,0,0));
  (code.(291) <- RETURN(1));
  (code.(292) <- GROUP4(RESTART()));
  (code.(293) <- GRAB(1));
  (code.(294) <- GROUP1(ACC(),1));
  (code.(295) <- GROUP1(UNOP(VECTLENGTH()),0));
  (code.(296) <- GROUP1(PUSH(),0));
  (code.(297) <- GROUP1(ACC(),0));
  (code.(298) <- GROUP3(BCOMPARE(NEQ(),0,301)));
  (code.(299) <- MAKEBLOCK(true,false,1,0,0));
  (code.(300) <- RETURN(3));
  (code.(301) <- GROUP1(CONST(0),0));
  (code.(302) <- GROUP1(PUSH(),0));
  (code.(303) <- GROUP1(ACC(),3));
  (code.(304) <- (CALL(17,true,false,false,false)));
  (code.(305) <- GROUP1(PUSH(),0));
  (code.(306) <- GROUP1(ACC(),2));
  (code.(307) <- APPLY(true,1,0));
  (code.(308) <- GROUP1(PUSH(),0));
  (code.(309) <- GROUP1(ACC(),1));
  (code.(310) <- (CALL(11,true,false,false,false)));
  (code.(311) <- GROUP1(PUSH(),0));
  (code.(312) <- GROUP1(CONST(1),0));
  (code.(313) <- GROUP1(PUSH(),0));
  (code.(314) <- GROUP1(ACC(),2));
  (code.(315) <- GROUP1(OFFSET(-1),0));
  (code.(316) <- GROUP1(PUSH(),0));
  (code.(317) <- GROUP1(PUSH(),0));
  (code.(318) <- GROUP1(ACC(),2));
  (code.(319) <- GROUP2(COMPARE(GT())));
  (code.(320) <- GROUP3(BRANCHIF(true,341)));
  (code.(321) <- GROUP3(CHECK_SIGNALS()));
  (code.(322) <- GROUP1(ACC(),1));
  (code.(323) <- GROUP1(PUSH(),0));
  (code.(324) <- GROUP1(ACC(),6));
  (code.(325) <- (CALL(17,true,false,false,false)));
  (code.(326) <- GROUP1(PUSH(),0));
  (code.(327) <- GROUP1(ACC(),5));
  (code.(328) <- APPLY(true,1,0));
  (code.(329) <- GROUP1(PUSH(),0));
  (code.(330) <- GROUP1(ACC(),2));
  (code.(331) <- GROUP1(PUSH(),0));
  (code.(332) <- GROUP1(ACC(),4));
  (code.(333) <- (CALL(16,true,true,false,false)));
  (code.(334) <- GROUP1(ACC(),1));
  (code.(335) <- GROUP1(PUSH(),0));
  (code.(336) <- GROUP1(OFFSET(1),0));
  (code.(337) <- GROUP1(ASSIGN(),2));
  (code.(338) <- GROUP1(ACC(),1));
  (code.(339) <- GROUP2(COMPARE(NEQ())));
  (code.(340) <- GROUP3(BRANCHIF(true,321)));
  (code.(341) <- GROUP1(CONST(0),0));
  (code.(342) <- GROUP1(POP(),2));
  (code.(343) <- GROUP1(ACC(),0));
  (code.(344) <- RETURN(4));
  (code.(345) <- GROUP4(RESTART()));
  (code.(346) <- GRAB(2));
  (code.(347) <- GROUP1(ACC(),1));
  (code.(348) <- GROUP1(PUSH(),0));
  (code.(349) <- GROUP1(CONST(0),0));
  (code.(350) <- GROUP2(COMPARE(GT())));
  (code.(351) <- GROUP3(BRANCHIF(false,356)));
  (code.(352) <- GROUP1(GETGLOBAL(),20));
  (code.(353) <- GROUP1(PUSH(),0));
  (code.(354) <- GROUP1(GETGLOBAL(),16));
  (code.(355) <- APPLY(true,1,0));
  (code.(356) <- MAKEBLOCK(true,false,1,0,0));
  (code.(357) <- GROUP1(PUSH(),0));
  (code.(358) <- GROUP1(ACC(),1));
  (code.(359) <- (CALL(11,true,false,false,false)));
  (code.(360) <- GROUP1(PUSH(),0));
  (code.(361) <- GROUP1(ACC(),2));
  (code.(362) <- GROUP1(PUSH(),0));
  (code.(363) <- GROUP1(CONST(0),0));
  (code.(364) <- GROUP2(COMPARE(LT())));
  (code.(365) <- GROUP3(BRANCHIF(false,394)));
  (code.(366) <- GROUP1(CONST(0),0));
  (code.(367) <- GROUP1(PUSH(),0));
  (code.(368) <- GROUP1(ACC(),2));
  (code.(369) <- GROUP1(OFFSET(-1),0));
  (code.(370) <- GROUP1(PUSH(),0));
  (code.(371) <- GROUP1(PUSH(),0));
  (code.(372) <- GROUP1(ACC(),2));
  (code.(373) <- GROUP2(COMPARE(GT())));
  (code.(374) <- GROUP3(BRANCHIF(true,392)));
  (code.(375) <- GROUP3(CHECK_SIGNALS()));
  (code.(376) <- GROUP1(ACC(),5));
  (code.(377) <- GROUP1(PUSH(),0));
  (code.(378) <- GROUP1(ACC(),5));
  (code.(379) <- (CALL(11,true,false,false,false)));
  (code.(380) <- GROUP1(PUSH(),0));
  (code.(381) <- GROUP1(ACC(),2));
  (code.(382) <- GROUP1(PUSH(),0));
  (code.(383) <- GROUP1(ACC(),4));
  (code.(384) <- GROUP2(SETVECTITEM()));
  (code.(385) <- GROUP1(ACC(),1));
  (code.(386) <- GROUP1(PUSH(),0));
  (code.(387) <- GROUP1(OFFSET(1),0));
  (code.(388) <- GROUP1(ASSIGN(),2));
  (code.(389) <- GROUP1(ACC(),1));
  (code.(390) <- GROUP2(COMPARE(NEQ())));
  (code.(391) <- GROUP3(BRANCHIF(true,375)));
  (code.(392) <- GROUP1(CONST(0),0));
  (code.(393) <- GROUP1(POP(),2));
  (code.(394) <- GROUP1(ACC(),0));
  (code.(395) <- RETURN(4));
  (code.(396) <- GROUP1(ACC(),0));
  (code.(397) <- GROUP1(UNOP(VECTLENGTH()),0));
  (code.(398) <- GROUP1(PUSH(),0));
  (code.(399) <- GROUP1(ACC(),0));
  (code.(400) <- GROUP3(BCOMPARE(NEQ(),0,403)));
  (code.(401) <- MAKEBLOCK(true,false,1,0,0));
  (code.(402) <- RETURN(2));
  (code.(403) <- GROUP1(ACC(),0));
  (code.(404) <- GROUP1(PUSH(),0));
  (code.(405) <- GROUP1(CONST(0),0));
  (code.(406) <- GROUP1(PUSH(),0));
  (code.(407) <- GROUP1(ACC(),3));
  (code.(408) <- (CALL(30,true,true,false,false)));
  (code.(409) <- RETURN(2));
  (code.(410) <- MAKEBLOCK(false,true,0,closure_tag,396));
  (code.(411) <- GROUP1(SETGLOBAL(),60));
  (code.(412) <- MAKEBLOCK(false,true,0,closure_tag,346));
  (code.(413) <- GROUP1(SETGLOBAL(),65));
  (code.(414) <- MAKEBLOCK(false,true,0,closure_tag,293));
  (code.(415) <- GROUP1(SETGLOBAL(),61));
  (code.(416) <- CLOSUREREC(1,0,2567,234));
  (code.(417) <- GROUP1(ACC(),0));
  (code.(418) <- GROUP1(SETGLOBAL(),19));
  (code.(419) <- MAKEBLOCK(false,true,0,closure_tag,268));
  (code.(420) <- GROUP1(SETGLOBAL(),82));
  (code.(421) <- GROUP1(CONST(0),0));
  (code.(422) <- (CALL(10,false,false,false,false)));
  (code.(423) <- GROUP1(CONST(0),0));
  (code.(424) <- (CALL(10,false,false,false,false)));
  (code.(425) <- GROUP3(BRANCH(2174)));
  (code.(426) <- GROUP4(RESTART()));
  (code.(427) <- GRAB(1));
  (code.(428) <- GROUP1(GETGLOBAL(),21));
  (code.(429) <- GROUP1(PUSH(),0));
  (code.(430) <- GROUP1(ACC(),1));
  (code.(431) <- GROUP3(BRANCHIF(false,434)));
  (code.(432) <- GROUP1(GETGLOBAL(),22));
  (code.(433) <- GROUP3(BRANCH(435)));
  (code.(434) <- GROUP1(GETGLOBAL(),23));
  (code.(435) <- GROUP1(PUSH(),0));
  (code.(436) <- GROUP1(GETGLOBAL(),24));
  (code.(437) <- APPLY(true,2,0));
  (code.(438) <- GROUP1(PUSH(),0));
  (code.(439) <- GROUP1(GETGLOBAL(),25));
  (code.(440) <- GROUP1(PUSH(),0));
  (code.(441) <- GROUP1(GETGLOBAL(),24));
  (code.(442) <- APPLY(true,2,0));
  (code.(443) <- (CALL(1,false,false,false,false)));
  (code.(444) <- GROUP1(ACC(),1));
  (code.(445) <- GROUP1(PUSH(),0));
  (code.(446) <- GROUP1(ACC(),1));
  (code.(447) <- GROUP1(PUSH(),0));
  (code.(448) <- GROUP1(GETGLOBAL(),26));
  (code.(449) <- GROUP1(GETFIELD(),3));
  (code.(450) <- APPLY(true,2,0));
  (code.(451) <- GROUP1(PUSH(),0));
  (code.(452) <- PUSHTRAP(512));
  (code.(453) <- GROUP3(BRANCH(504)));
  (code.(454) <- GROUP3(CHECK_SIGNALS()));
  (code.(455) <- GROUP1(CONST(0),0));
  (code.(456) <- GROUP1(PUSH(),0));
  (code.(457) <- GROUP1(GETGLOBAL(),27));
  (code.(458) <- APPLY(true,1,0));
  (code.(459) <- GROUP1(PUSH(),0));
  (code.(460) <- GROUP1(ACC(),0));
  (code.(461) <- (CALL(25,false,false,false,false)));
  (code.(462) <- GROUP1(PUSH(),0));
  (code.(463) <- GROUP1(CONST(0),0));
  (code.(464) <- GROUP2(COMPARE(LT())));
  (code.(465) <- GROUP3(BRANCHIF(false,473)));
  (code.(466) <- GROUP1(CONST(0),0));
  (code.(467) <- GROUP1(PUSH(),0));
  (code.(468) <- GROUP1(ACC(),1));
  (code.(469) <- (CALL(31,true,false,false,false)));
  (code.(470) <- GROUP1(PUSH(),0));
  (code.(471) <- GROUP1(GETGLOBAL(),28));
  (code.(472) <- APPLY(true,1,0));
  (code.(473) <- GROUP3(BRANCHIF(false,501)));
  (code.(474) <- GROUP1(CONST(48),0));
  (code.(475) <- GROUP1(PUSH(),0));
  (code.(476) <- GROUP1(GETGLOBAL(),29));
  (code.(477) <- APPLY(true,1,0));
  (code.(478) <- GROUP1(PUSH(),0));
  (code.(479) <- GROUP1(CONST(0),0));
  (code.(480) <- GROUP1(PUSH(),0));
  (code.(481) <- GROUP1(ACC(),2));
  (code.(482) <- (CALL(31,true,false,false,false)));
  (code.(483) <- GROUP1(PUSH(),0));
  (code.(484) <- GROUP1(GETGLOBAL(),29));
  (code.(485) <- APPLY(true,1,0));
  (code.(486) <- GROUP2(SUB()));
  (code.(487) <- GROUP1(PUSH(),0));
  (code.(488) <- GROUP1(ACC(),6));
  (code.(489) <- GROUP1(PUSH(),0));
  (code.(490) <- GROUP1(ACC(),1));
  (code.(491) <- GROUP1(PUSH(),0));
  (code.(492) <- GROUP1(GETGLOBAL(),30));
  (code.(493) <- APPLY(true,2,0));
  (code.(494) <- GROUP3(BRANCHIF(false,500)));
  (code.(495) <- GROUP1(ACC(),0));
  (code.(496) <- GROUP1(PUSH(),0));
  (code.(497) <- GROUP1(GETGLOBAL(),31));
  (code.(498) <- MAKEBLOCK(false,false,2,0,0));
  (code.(499) <- GROUP4(RAISE()));
  (code.(500) <- GROUP1(POP(),1));
  (code.(501) <- GROUP1(GETGLOBAL(),32));
  (code.(502) <- (CALL(1,false,false,false,false)));
  (code.(503) <- GROUP1(POP(),1));
  (code.(504) <- GROUP1(CONST(1),0));
  (code.(505) <- GROUP3(BRANCHIF(true,454)));
  (code.(506) <- GROUP1(ACC(),4));
  (code.(507) <- GROUP1(PUSH(),0));
  (code.(508) <- GROUP1(GETGLOBAL(),33));
  (code.(509) <- APPLY(true,1,0));
  (code.(510) <- GROUP4(POPTRAP()));
  (code.(511) <- RETURN(3));
  (code.(512) <- GROUP1(PUSH(),0));
  (code.(513) <- GROUP1(GETGLOBAL(),31));
  (code.(514) <- GROUP1(PUSH(),0));
  (code.(515) <- GROUP1(ACC(),1));
  (code.(516) <- GROUP1(GETFIELD(),0));
  (code.(517) <- GROUP2(COMPARE(EQ())));
  (code.(518) <- GROUP3(BRANCHIF(false,522)));
  (code.(519) <- GROUP1(ACC(),0));
  (code.(520) <- GROUP1(GETFIELD(),1));
  (code.(521) <- RETURN(4));
  (code.(522) <- GROUP1(ACC(),1));
  (code.(523) <- GROUP1(PUSH(),0));
  (code.(524) <- GROUP1(GETGLOBAL(),33));
  (code.(525) <- APPTERM(1,5));
  (code.(526) <- GROUP1(ACC(),0));
  (code.(527) <- GROUP1(OFFSET(-49),0));
  (code.(528) <- GROUP3(BCOMPARE(LE(),6,531)));
  (code.(529) <- GROUP1(CONST(0),0));
  (code.(530) <- RETURN(1));
  (code.(531) <- GROUP1(CONST(1),0));
  (code.(532) <- RETURN(1));
  (code.(533) <- GROUP4(RESTART()));
  (code.(534) <- GRAB(3));
  (code.(535) <- GROUP1(ACC(),3));
  (code.(536) <- GROUP1(PUSH(),0));
  (code.(537) <- GROUP1(GETGLOBAL(),34));
  (code.(538) <- APPTERM(1,5));
  (code.(539) <- GROUP1(GETGLOBAL(),35));
  (code.(540) <- (CALL(1,false,false,false,false)));
  (code.(541) <- GROUP1(CONST(0),0));
  (code.(542) <- (CALL(33,false,false,false,false)));
  (code.(543) <- GROUP1(CONST(0),0));
  (code.(544) <- (CALL(33,false,false,false,false)));
  (code.(545) <- GROUP1(CONST(0),0));
  (code.(546) <- GROUP1(PUSH(),0));
  (code.(547) <- GROUP1(GETGLOBAL(),26));
  (code.(548) <- GROUP1(GETFIELD(),2));
  (code.(549) <- APPLY(true,1,0));
  (code.(550) <- GROUP1(PUSH(),0));
  (code.(551) <- GROUP1(GETGLOBAL(),34));
  (code.(552) <- APPLY(true,1,0));
  (code.(553) <- GROUP1(CONST(0),0));
  (code.(554) <- (CALL(33,false,false,false,false)));
  (code.(555) <- RETURN(1));
  (code.(556) <- GROUP1(GETGLOBAL(),36));
  (code.(557) <- (CALL(1,false,false,false,false)));
  (code.(558) <- GROUP1(CONST(0),0));
  (code.(559) <- (CALL(33,false,false,false,false)));
  (code.(560) <- RETURN(1));
  (code.(561) <- GROUP1(GETGLOBAL(),37));
  (code.(562) <- (CALL(1,false,false,false,false)));
  (code.(563) <- GROUP1(CONST(0),0));
  (code.(564) <- (CALL(33,false,false,false,false)));
  (code.(565) <- RETURN(1));
  (code.(566) <- GROUP1(GETGLOBAL(),38));
  (code.(567) <- (CALL(1,false,false,false,false)));
  (code.(568) <- GROUP1(CONST(0),0));
  (code.(569) <- (CALL(33,false,false,false,false)));
  (code.(570) <- RETURN(1));
  (code.(571) <- GROUP1(GETGLOBAL(),39));
  (code.(572) <- GROUP1(PUSH(),0));
  (code.(573) <- GROUP1(GETGLOBAL(),40));
  (code.(574) <- APPTERM(1,2));
  (code.(575) <- GROUP1(GETGLOBAL(),41));
  (code.(576) <- GROUP1(PUSH(),0));
  (code.(577) <- GROUP1(GETGLOBAL(),40));
  (code.(578) <- APPTERM(1,2));
  (code.(579) <- GROUP1(GETGLOBAL(),42));
  (code.(580) <- GROUP1(PUSH(),0));
  (code.(581) <- GROUP1(GETGLOBAL(),40));
  (code.(582) <- APPTERM(1,2));
  (code.(583) <- GROUP1(ACC(),0));
  (code.(584) <- (CALL(1,false,false,false,false)));
  (code.(585) <- GROUP1(GETGLOBAL(),43));
  (code.(586) <- (CALL(1,false,false,false,false)));
  (code.(587) <- GROUP1(GETGLOBAL(),44));
  (code.(588) <- GROUP1(PUSH(),0));
  (code.(589) <- GROUP1(CONST(0),0));
  (code.(590) <- GROUP1(PUSH(),0));
  (code.(591) <- GROUP1(GETGLOBAL(),27));
  (code.(592) <- APPLY(true,1,0));
  (code.(593) <- (CALL(32,true,false,false,false)));
  (code.(594) <- RETURN(1));
  (code.(595) <- GROUP1(GETGLOBAL(),45));
  (code.(596) <- (CALL(1,false,false,false,false)));
  (code.(597) <- RETURN(1));
  (code.(598) <- GROUP1(GETGLOBAL(),46));
  (code.(599) <- (CALL(1,false,false,false,false)));
  (code.(600) <- RETURN(1));
  (code.(601) <- GROUP1(GETGLOBAL(),26));
  (code.(602) <- GROUP1(GETFIELD(),1));
  (code.(603) <- GROUP1(OFFSET(-1),0));
  (code.(604) <- GROUP1(PUSH(),0));
  (code.(605) <- GROUP1(CONST(0),0));
  (code.(606) <- GROUP1(PUSH(),0));
  (code.(607) <- GROUP1(PUSH(),0));
  (code.(608) <- GROUP1(ACC(),2));
  (code.(609) <- GROUP2(COMPARE(LT())));
  (code.(610) <- GROUP3(BRANCHIF(true,660)));
  (code.(611) <- GROUP3(CHECK_SIGNALS()));
  (code.(612) <- GROUP1(CONST(0),0));
  (code.(613) <- GROUP1(PUSH(),0));
  (code.(614) <- GROUP1(GETGLOBAL(),26));
  (code.(615) <- GROUP1(GETFIELD(),0));
  (code.(616) <- GROUP1(OFFSET(-1),0));
  (code.(617) <- GROUP1(PUSH(),0));
  (code.(618) <- GROUP1(PUSH(),0));
  (code.(619) <- GROUP1(ACC(),2));
  (code.(620) <- GROUP2(COMPARE(GT())));
  (code.(621) <- GROUP3(BRANCHIF(true,649)));
  (code.(622) <- GROUP3(CHECK_SIGNALS()));
  (code.(623) <- GROUP1(ACC(),1));
  (code.(624) <- GROUP1(PUSH(),0));
  (code.(625) <- GROUP1(ACC(),4));
  (code.(626) <- GROUP1(PUSH(),0));
  (code.(627) <- GROUP1(ACC(),6));
  (code.(628) <- (CALL(18,true,false,false,false)));
  (code.(629) <- (CALL(18,true,false,false,false)));
  (code.(630) <- GROUP1(PUSH(),0));
  (code.(631) <- GROUP1(ACC(),0));
  (code.(632) <- GROUP3(SWITCH(2568,2571)));
  (code.(633) <- GROUP1(GETGLOBAL(),47));
  (code.(634) <- (CALL(1,false,false,false,false)));
  (code.(635) <- GROUP3(BRANCH(641)));
  (code.(636) <- GROUP1(GETGLOBAL(),48));
  (code.(637) <- (CALL(1,false,false,false,false)));
  (code.(638) <- GROUP3(BRANCH(641)));
  (code.(639) <- GROUP1(GETGLOBAL(),49));
  (code.(640) <- (CALL(1,false,false,false,false)));
  (code.(641) <- GROUP1(POP(),1));
  (code.(642) <- GROUP1(ACC(),1));
  (code.(643) <- GROUP1(PUSH(),0));
  (code.(644) <- GROUP1(OFFSET(1),0));
  (code.(645) <- GROUP1(ASSIGN(),2));
  (code.(646) <- GROUP1(ACC(),1));
  (code.(647) <- GROUP2(COMPARE(NEQ())));
  (code.(648) <- GROUP3(BRANCHIF(true,622)));
  (code.(649) <- GROUP1(CONST(0),0));
  (code.(650) <- GROUP1(POP(),2));
  (code.(651) <- GROUP1(CONST(0),0));
  (code.(652) <- (CALL(33,false,false,false,false)));
  (code.(653) <- GROUP1(ACC(),1));
  (code.(654) <- GROUP1(PUSH(),0));
  (code.(655) <- GROUP1(OFFSET(-1),0));
  (code.(656) <- GROUP1(ASSIGN(),2));
  (code.(657) <- GROUP1(ACC(),1));
  (code.(658) <- GROUP2(COMPARE(NEQ())));
  (code.(659) <- GROUP3(BRANCHIF(true,611)));
  (code.(660) <- GROUP1(CONST(0),0));
  (code.(661) <- GROUP1(POP(),2));
  (code.(662) <- GROUP1(CONST(0),0));
  (code.(663) <- (CALL(33,false,false,false,false)));
  (code.(664) <- RETURN(1));
  (code.(665) <- GROUP4(RESTART()));
  (code.(666) <- GRAB(1));
  (code.(667) <- GROUP1(ACC(),1));
  (code.(668) <- GROUP1(PUSH(),0));
  (code.(669) <- GROUP1(ACC(),1));
  (code.(670) <- GROUP1(PUSH(),0));
  (code.(671) <- GROUP1(GETGLOBAL(),50));
  (code.(672) <- APPLY(true,2,0));
  (code.(673) <- GROUP1(PUSH(),0));
  (code.(674) <- GROUP1(GETGLOBAL(),51));
  (code.(675) <- GROUP1(PUSH(),0));
  (code.(676) <- GROUP1(ACC(),1));
  (code.(677) <- GROUP2(COMPARE(EQ())));
  (code.(678) <- GROUP3(BRANCHIF(false,685)));
  (code.(679) <- GROUP1(ACC(),1));
  (code.(680) <- GROUP3(BRANCHIF(false,683)));
  (code.(681) <- GROUP1(CONST(0),0));
  (code.(682) <- RETURN(3));
  (code.(683) <- GROUP1(CONST(1),0));
  (code.(684) <- RETURN(3));
  (code.(685) <- GROUP1(GETGLOBAL(),52));
  (code.(686) <- GROUP1(PUSH(),0));
  (code.(687) <- GROUP1(ACC(),1));
  (code.(688) <- GROUP2(COMPARE(EQ())));
  (code.(689) <- GROUP3(BRANCHIF(false,694)));
  (code.(690) <- GROUP1(ACC(),1));
  (code.(691) <- GROUP3(BRANCHIF(false,693)));
  (code.(692) <- GROUP1(CONST(1),0));
  (code.(693) <- RETURN(3));
  (code.(694) <- GROUP1(ACC(),2));
  (code.(695) <- GROUP1(PUSH(),0));
  (code.(696) <- GROUP1(ACC(),2));
  (code.(697) <- GROUP1(PUSH(),0));
  (code.(698) <- GROUP1(GETGLOBAL(),26));
  (code.(699) <- GROUP1(GETFIELD(),3));
  (code.(700) <- APPLY(true,2,0));
  (code.(701) <- GROUP3(BCOMPARE(NEQ(),0,704)));
  (code.(702) <- GROUP1(CONST(2),0));
  (code.(703) <- RETURN(3));
  (code.(704) <- GROUP1(CONST(3),0));
  (code.(705) <- RETURN(3));
  (code.(706) <- GROUP4(RESTART()));
  (code.(707) <- GRAB(1));
  (code.(708) <- GROUP1(CONST(1),0));
  (code.(709) <- RETURN(2));
  (code.(710) <- GROUP4(RESTART()));
  (code.(711) <- GRAB(1));
  (code.(712) <- GROUP1(ACC(),1));
  (code.(713) <- GROUP1(PUSH(),0));
  (code.(714) <- GROUP1(ACC(),1));
  (code.(715) <- GROUP1(PUSH(),0));
  (code.(716) <- GROUP1(GETGLOBAL(),50));
  (code.(717) <- APPLY(true,2,0));
  (code.(718) <- GROUP1(PUSH(),0));
  (code.(719) <- GROUP1(GETGLOBAL(),51));
  (code.(720) <- GROUP1(PUSH(),0));
  (code.(721) <- GROUP1(ACC(),1));
  (code.(722) <- GROUP2(COMPARE(EQ())));
  (code.(723) <- GROUP3(BRANCHIF(true,739)));
  (code.(724) <- GROUP1(GETGLOBAL(),52));
  (code.(725) <- GROUP1(PUSH(),0));
  (code.(726) <- GROUP1(ACC(),1));
  (code.(727) <- GROUP2(COMPARE(EQ())));
  (code.(728) <- GROUP3(BRANCHIF(true,739)));
  (code.(729) <- GROUP1(ACC(),2));
  (code.(730) <- GROUP1(PUSH(),0));
  (code.(731) <- GROUP1(ACC(),2));
  (code.(732) <- GROUP1(PUSH(),0));
  (code.(733) <- GROUP1(GETGLOBAL(),26));
  (code.(734) <- GROUP1(GETFIELD(),3));
  (code.(735) <- APPLY(true,2,0));
  (code.(736) <- GROUP1(PUSH(),0));
  (code.(737) <- GROUP1(CONST(0),0));
  (code.(738) <- GROUP2(COMPARE(EQ())));
  (code.(739) <- RETURN(3));
  (code.(740) <- GROUP4(RESTART()));
  (code.(741) <- GRAB(1));
  (code.(742) <- PUSHTRAP(889));
  (code.(743) <- GROUP1(CONST(0),0));
  (code.(744) <- MAKEBLOCK(false,false,1,0,0));
  (code.(745) <- GROUP1(PUSH(),0));
  (code.(746) <- GROUP1(PUSHRETADDR(),769));
  (code.(747) <- GROUP1(CONST(1),0));
  (code.(748) <- GROUP1(PUSH(),0));
  (code.(749) <- GROUP1(CONST(0),0));
  (code.(750) <- GROUP1(PUSH(),0));
  (code.(751) <- GROUP1(GETGLOBAL(),26));
  (code.(752) <- GROUP1(GETFIELD(),0));
  (code.(753) <- GROUP1(OFFSET(-4),0));
  (code.(754) <- GROUP1(PUSH(),0));
  (code.(755) <- GROUP1(CONST(0),0));
  (code.(756) <- GROUP1(PUSH(),0));
  (code.(757) <- GROUP1(GETGLOBAL(),26));
  (code.(758) <- GROUP1(GETFIELD(),1));
  (code.(759) <- GROUP1(OFFSET(-1),0));
  (code.(760) <- GROUP1(PUSH(),0));
  (code.(761) <- GROUP1(CONST(0),0));
  (code.(762) <- GROUP1(PUSH(),0));
  (code.(763) <- GROUP1(ACC(),9));
  (code.(764) <- GROUP1(PUSH(),0));
  (code.(765) <- GROUP1(ACC(),16));
  (code.(766) <- GROUP1(PUSH(),0));
  (code.(767) <- GROUP1(GETGLOBAL(),53));
  (code.(768) <- APPLY(false,8,0));
  (code.(769) <- GROUP1(PUSHRETADDR(),792));
  (code.(770) <- GROUP1(CONST(0),0));
  (code.(771) <- GROUP1(PUSH(),0));
  (code.(772) <- GROUP1(CONST(1),0));
  (code.(773) <- GROUP1(PUSH(),0));
  (code.(774) <- GROUP1(GETGLOBAL(),26));
  (code.(775) <- GROUP1(GETFIELD(),1));
  (code.(776) <- GROUP1(OFFSET(-4),0));
  (code.(777) <- GROUP1(PUSH(),0));
  (code.(778) <- GROUP1(CONST(0),0));
  (code.(779) <- GROUP1(PUSH(),0));
  (code.(780) <- GROUP1(GETGLOBAL(),26));
  (code.(781) <- GROUP1(GETFIELD(),0));
  (code.(782) <- GROUP1(OFFSET(-1),0));
  (code.(783) <- GROUP1(PUSH(),0));
  (code.(784) <- GROUP1(CONST(0),0));
  (code.(785) <- GROUP1(PUSH(),0));
  (code.(786) <- GROUP1(ACC(),9));
  (code.(787) <- GROUP1(PUSH(),0));
  (code.(788) <- GROUP1(ACC(),16));
  (code.(789) <- GROUP1(PUSH(),0));
  (code.(790) <- GROUP1(GETGLOBAL(),53));
  (code.(791) <- APPLY(false,8,0));
  (code.(792) <- GROUP1(PUSHRETADDR(),815));
  (code.(793) <- GROUP1(CONST(1),0));
  (code.(794) <- GROUP1(PUSH(),0));
  (code.(795) <- GROUP1(CONST(1),0));
  (code.(796) <- GROUP1(PUSH(),0));
  (code.(797) <- GROUP1(GETGLOBAL(),26));
  (code.(798) <- GROUP1(GETFIELD(),1));
  (code.(799) <- GROUP1(OFFSET(-4),0));
  (code.(800) <- GROUP1(PUSH(),0));
  (code.(801) <- GROUP1(CONST(0),0));
  (code.(802) <- GROUP1(PUSH(),0));
  (code.(803) <- GROUP1(GETGLOBAL(),26));
  (code.(804) <- GROUP1(GETFIELD(),0));
  (code.(805) <- GROUP1(OFFSET(-4),0));
  (code.(806) <- GROUP1(PUSH(),0));
  (code.(807) <- GROUP1(CONST(0),0));
  (code.(808) <- GROUP1(PUSH(),0));
  (code.(809) <- GROUP1(ACC(),9));
  (code.(810) <- GROUP1(PUSH(),0));
  (code.(811) <- GROUP1(ACC(),16));
  (code.(812) <- GROUP1(PUSH(),0));
  (code.(813) <- GROUP1(GETGLOBAL(),53));
  (code.(814) <- APPLY(false,8,0));
  (code.(815) <- GROUP1(PUSHRETADDR(),838));
  (code.(816) <- GROUP1(CONST(1),0));
  (code.(817) <- GROUP1(PUSH(),0));
  (code.(818) <- GROUP1(CONST(1),0));
  (code.(819) <- GROUP1(PUSH(),0));
  (code.(820) <- GROUP1(GETGLOBAL(),26));
  (code.(821) <- GROUP1(GETFIELD(),0));
  (code.(822) <- GROUP1(OFFSET(-4),0));
  (code.(823) <- GROUP1(PUSH(),0));
  (code.(824) <- GROUP1(CONST(0),0));
  (code.(825) <- GROUP1(PUSH(),0));
  (code.(826) <- GROUP1(GETGLOBAL(),26));
  (code.(827) <- GROUP1(GETFIELD(),1));
  (code.(828) <- GROUP1(OFFSET(-4),0));
  (code.(829) <- GROUP1(PUSH(),0));
  (code.(830) <- GROUP1(CONST(1),0));
  (code.(831) <- GROUP1(PUSH(),0));
  (code.(832) <- GROUP1(ACC(),9));
  (code.(833) <- GROUP1(PUSH(),0));
  (code.(834) <- GROUP1(ACC(),16));
  (code.(835) <- GROUP1(PUSH(),0));
  (code.(836) <- GROUP1(GETGLOBAL(),53));
  (code.(837) <- APPLY(false,8,0));
  (code.(838) <- GROUP1(PUSHRETADDR(),861));
  (code.(839) <- GROUP1(CONST(-1),0));
  (code.(840) <- GROUP1(PUSH(),0));
  (code.(841) <- GROUP1(CONST(1),0));
  (code.(842) <- GROUP1(PUSH(),0));
  (code.(843) <- GROUP1(GETGLOBAL(),26));
  (code.(844) <- GROUP1(GETFIELD(),1));
  (code.(845) <- GROUP1(OFFSET(-4),0));
  (code.(846) <- GROUP1(PUSH(),0));
  (code.(847) <- GROUP1(CONST(0),0));
  (code.(848) <- GROUP1(PUSH(),0));
  (code.(849) <- GROUP1(GETGLOBAL(),26));
  (code.(850) <- GROUP1(GETFIELD(),0));
  (code.(851) <- GROUP1(OFFSET(-1),0));
  (code.(852) <- GROUP1(PUSH(),0));
  (code.(853) <- GROUP1(CONST(3),0));
  (code.(854) <- GROUP1(PUSH(),0));
  (code.(855) <- GROUP1(ACC(),9));
  (code.(856) <- GROUP1(PUSH(),0));
  (code.(857) <- GROUP1(ACC(),16));
  (code.(858) <- GROUP1(PUSH(),0));
  (code.(859) <- GROUP1(GETGLOBAL(),53));
  (code.(860) <- APPLY(false,8,0));
  (code.(861) <- GROUP1(PUSHRETADDR(),884));
  (code.(862) <- GROUP1(CONST(-1),0));
  (code.(863) <- GROUP1(PUSH(),0));
  (code.(864) <- GROUP1(CONST(1),0));
  (code.(865) <- GROUP1(PUSH(),0));
  (code.(866) <- GROUP1(GETGLOBAL(),26));
  (code.(867) <- GROUP1(GETFIELD(),0));
  (code.(868) <- GROUP1(OFFSET(-1),0));
  (code.(869) <- GROUP1(PUSH(),0));
  (code.(870) <- GROUP1(CONST(3),0));
  (code.(871) <- GROUP1(PUSH(),0));
  (code.(872) <- GROUP1(GETGLOBAL(),26));
  (code.(873) <- GROUP1(GETFIELD(),1));
  (code.(874) <- GROUP1(OFFSET(-4),0));
  (code.(875) <- GROUP1(PUSH(),0));
  (code.(876) <- GROUP1(CONST(1),0));
  (code.(877) <- GROUP1(PUSH(),0));
  (code.(878) <- GROUP1(ACC(),9));
  (code.(879) <- GROUP1(PUSH(),0));
  (code.(880) <- GROUP1(ACC(),16));
  (code.(881) <- GROUP1(PUSH(),0));
  (code.(882) <- GROUP1(GETGLOBAL(),53));
  (code.(883) <- APPLY(false,8,0));
  (code.(884) <- GROUP1(ACC(),0));
  (code.(885) <- GROUP1(GETFIELD(),0));
  (code.(886) <- GROUP1(POP(),1));
  (code.(887) <- GROUP4(POPTRAP()));
  (code.(888) <- RETURN(2));
  (code.(889) <- GROUP1(PUSH(),0));
  (code.(890) <- GROUP1(GETGLOBAL(),54));
  (code.(891) <- GROUP1(PUSH(),0));
  (code.(892) <- GROUP1(ACC(),1));
  (code.(893) <- GROUP1(GETFIELD(),0));
  (code.(894) <- GROUP2(COMPARE(EQ())));
  (code.(895) <- GROUP3(BRANCHIF(false,899)));
  (code.(896) <- GROUP1(ACC(),0));
  (code.(897) <- GROUP1(GETFIELD(),1));
  (code.(898) <- RETURN(3));
  (code.(899) <- GROUP1(ACC(),0));
  (code.(900) <- GROUP4(RAISE()));
  (code.(901) <- GROUP4(RESTART()));
  (code.(902) <- GRAB(7));
  (code.(903) <- GROUP1(ACC(),2));
  (code.(904) <- GROUP1(PUSH(),0));
  (code.(905) <- GROUP1(ACC(),4));
  (code.(906) <- GROUP1(PUSH(),0));
  (code.(907) <- GROUP1(PUSH(),0));
  (code.(908) <- GROUP1(ACC(),2));
  (code.(909) <- GROUP2(COMPARE(GT())));
  (code.(910) <- GROUP3(BRANCHIF(true,957)));
  (code.(911) <- GROUP3(CHECK_SIGNALS()));
  (code.(912) <- GROUP1(ACC(),6));
  (code.(913) <- GROUP1(PUSH(),0));
  (code.(914) <- GROUP1(ACC(),8));
  (code.(915) <- GROUP1(PUSH(),0));
  (code.(916) <- GROUP1(PUSH(),0));
  (code.(917) <- GROUP1(ACC(),2));
  (code.(918) <- GROUP2(COMPARE(GT())));
  (code.(919) <- GROUP3(BRANCHIF(true,948)));
  (code.(920) <- GROUP3(CHECK_SIGNALS()));
  (code.(921) <- GROUP1(PUSHRETADDR(),934));
  (code.(922) <- GROUP1(ACC(),14));
  (code.(923) <- GROUP1(PUSH(),0));
  (code.(924) <- GROUP1(ACC(),14));
  (code.(925) <- GROUP1(PUSH(),0));
  (code.(926) <- GROUP1(ACC(),8));
  (code.(927) <- GROUP1(PUSH(),0));
  (code.(928) <- GROUP1(ACC(),7));
  (code.(929) <- GROUP1(PUSH(),0));
  (code.(930) <- GROUP1(ACC(),11));
  (code.(931) <- GROUP1(PUSH(),0));
  (code.(932) <- GROUP1(GETGLOBAL(),55));
  (code.(933) <- APPLY(false,5,0));
  (code.(934) <- GROUP1(PUSH(),0));
  (code.(935) <- GROUP1(ACC(),6));
  (code.(936) <- GROUP1(GETFIELD(),0));
  (code.(937) <- GROUP2(ADD()));
  (code.(938) <- GROUP1(PUSH(),0));
  (code.(939) <- GROUP1(ACC(),6));
  (code.(940) <- GROUP2(SETFIELD(0)));
  (code.(941) <- GROUP1(ACC(),1));
  (code.(942) <- GROUP1(PUSH(),0));
  (code.(943) <- GROUP1(OFFSET(1),0));
  (code.(944) <- GROUP1(ASSIGN(),2));
  (code.(945) <- GROUP1(ACC(),1));
  (code.(946) <- GROUP2(COMPARE(NEQ())));
  (code.(947) <- GROUP3(BRANCHIF(true,920)));
  (code.(948) <- GROUP1(CONST(0),0));
  (code.(949) <- GROUP1(POP(),2));
  (code.(950) <- GROUP1(ACC(),1));
  (code.(951) <- GROUP1(PUSH(),0));
  (code.(952) <- GROUP1(OFFSET(1),0));
  (code.(953) <- GROUP1(ASSIGN(),2));
  (code.(954) <- GROUP1(ACC(),1));
  (code.(955) <- GROUP2(COMPARE(NEQ())));
  (code.(956) <- GROUP3(BRANCHIF(true,911)));
  (code.(957) <- GROUP1(CONST(0),0));
  (code.(958) <- RETURN(10));
  (code.(959) <- GROUP4(RESTART()));
  (code.(960) <- GRAB(4));
  (code.(961) <- GROUP1(CONST(0),0));
  (code.(962) <- GROUP1(PUSH(),0));
  (code.(963) <- GROUP1(CONST(2),0));
  (code.(964) <- GROUP1(PUSH(),0));
  (code.(965) <- GROUP1(ACC(),4));
  (code.(966) <- GROUP1(PUSH(),0));
  (code.(967) <- GROUP1(ACC(),4));
  (code.(968) <- GROUP1(PUSH(),0));
  (code.(969) <- PUSHTRAP(1093));
  (code.(970) <- GROUP1(CONST(1),0));
  (code.(971) <- GROUP1(PUSH(),0));
  (code.(972) <- GROUP1(CONST(4),0));
  (code.(973) <- GROUP1(PUSH(),0));
  (code.(974) <- GROUP1(PUSH(),0));
  (code.(975) <- GROUP1(ACC(),2));
  (code.(976) <- GROUP2(COMPARE(GT())));
  (code.(977) <- GROUP3(BRANCHIF(true,1078)));
  (code.(978) <- GROUP3(CHECK_SIGNALS()));
  (code.(979) <- GROUP1(ACC(),6));
  (code.(980) <- GROUP1(PUSH(),0));
  (code.(981) <- GROUP1(CONST(0),0));
  (code.(982) <- GROUP2(COMPARE(GT())));
  (code.(983) <- GROUP3(BRANCHIF(true,1000)));
  (code.(984) <- GROUP1(GETGLOBAL(),26));
  (code.(985) <- GROUP1(GETFIELD(),1));
  (code.(986) <- GROUP1(PUSH(),0));
  (code.(987) <- GROUP1(ACC(),7));
  (code.(988) <- GROUP2(COMPARE(GE())));
  (code.(989) <- GROUP3(BRANCHIF(true,1000)));
  (code.(990) <- GROUP1(ACC(),7));
  (code.(991) <- GROUP1(PUSH(),0));
  (code.(992) <- GROUP1(CONST(0),0));
  (code.(993) <- GROUP2(COMPARE(GT())));
  (code.(994) <- GROUP3(BRANCHIF(true,1000)));
  (code.(995) <- GROUP1(GETGLOBAL(),26));
  (code.(996) <- GROUP1(GETFIELD(),0));
  (code.(997) <- GROUP1(PUSH(),0));
  (code.(998) <- GROUP1(ACC(),8));
  (code.(999) <- GROUP2(COMPARE(GE())));
  (code.(1000) <- GROUP3(BRANCHIF(false,1003)));
  (code.(1001) <- GROUP1(GETGLOBAL(),56));
  (code.(1002) <- GROUP4(RAISE()));
  (code.(1003) <- GROUP1(ACC(),7));
  (code.(1004) <- GROUP1(PUSH(),0));
  (code.(1005) <- GROUP1(ACC(),7));
  (code.(1006) <- GROUP1(PUSH(),0));
  (code.(1007) <- GROUP1(ACC(),12));
  (code.(1008) <- (CALL(18,true,false,false,false)));
  (code.(1009) <- (CALL(18,true,false,false,false)));
  (code.(1010) <- GROUP1(PUSH(),0));
  (code.(1011) <- GROUP1(ACC(),0));
  (code.(1012) <- GROUP3(SWITCH(2571,2574)));
  (code.(1013) <- GROUP1(ACC(),9));
  (code.(1014) <- GROUP1(PUSH(),0));
  (code.(1015) <- GROUP1(CONST(1),0));
  (code.(1016) <- GROUP2(COMPARE(EQ())));
  (code.(1017) <- GROUP3(BRANCHIF(false,1020)));
  (code.(1018) <- GROUP1(GETGLOBAL(),57));
  (code.(1019) <- GROUP4(RAISE()));
  (code.(1020) <- GROUP1(ACC(),10));
  (code.(1021) <- GROUP1(OFFSET(1),0));
  (code.(1022) <- GROUP1(ASSIGN(),10));
  (code.(1023) <- GROUP1(ACC(),10));
  (code.(1024) <- GROUP1(PUSH(),0));
  (code.(1025) <- GROUP1(CONST(4),0));
  (code.(1026) <- GROUP2(COMPARE(EQ())));
  (code.(1027) <- GROUP3(BRANCHIF(false,1033)));
  (code.(1028) <- GROUP1(GETGLOBAL(),51));
  (code.(1029) <- GROUP1(PUSH(),0));
  (code.(1030) <- GROUP1(GETGLOBAL(),54));
  (code.(1031) <- MAKEBLOCK(false,false,2,0,0));
  (code.(1032) <- GROUP4(RAISE()));
  (code.(1033) <- GROUP1(CONST(0),0));
  (code.(1034) <- GROUP1(ASSIGN(),9));
  (code.(1035) <- GROUP3(BRANCH(1060)));
  (code.(1036) <- GROUP1(ACC(),9));
  (code.(1037) <- GROUP1(PUSH(),0));
  (code.(1038) <- GROUP1(CONST(0),0));
  (code.(1039) <- GROUP2(COMPARE(EQ())));
  (code.(1040) <- GROUP3(BRANCHIF(false,1043)));
  (code.(1041) <- GROUP1(GETGLOBAL(),57));
  (code.(1042) <- GROUP4(RAISE()));
  (code.(1043) <- GROUP1(ACC(),10));
  (code.(1044) <- GROUP1(OFFSET(1),0));
  (code.(1045) <- GROUP1(ASSIGN(),10));
  (code.(1046) <- GROUP1(ACC(),10));
  (code.(1047) <- GROUP1(PUSH(),0));
  (code.(1048) <- GROUP1(CONST(4),0));
  (code.(1049) <- GROUP2(COMPARE(EQ())));
  (code.(1050) <- GROUP3(BRANCHIF(false,1056)));
  (code.(1051) <- GROUP1(GETGLOBAL(),52));
  (code.(1052) <- GROUP1(PUSH(),0));
  (code.(1053) <- GROUP1(GETGLOBAL(),54));
  (code.(1054) <- MAKEBLOCK(false,false,2,0,0));
  (code.(1055) <- GROUP4(RAISE()));
  (code.(1056) <- GROUP1(CONST(1),0));
  (code.(1057) <- GROUP1(ASSIGN(),9));
  (code.(1058) <- GROUP3(BRANCH(1060)));
  (code.(1059) <- GROUP1(CONST(0),0));
  (code.(1060) <- GROUP1(POP(),1));
  (code.(1061) <- GROUP1(ACC(),14));
  (code.(1062) <- GROUP1(PUSH(),0));
  (code.(1063) <- GROUP1(ACC(),8));
  (code.(1064) <- GROUP2(ADD()));
  (code.(1065) <- GROUP1(ASSIGN(),7));
  (code.(1066) <- GROUP1(ACC(),13));
  (code.(1067) <- GROUP1(PUSH(),0));
  (code.(1068) <- GROUP1(ACC(),7));
  (code.(1069) <- GROUP2(ADD()));
  (code.(1070) <- GROUP1(ASSIGN(),6));
  (code.(1071) <- GROUP1(ACC(),1));
  (code.(1072) <- GROUP1(PUSH(),0));
  (code.(1073) <- GROUP1(OFFSET(1),0));
  (code.(1074) <- GROUP1(ASSIGN(),2));
  (code.(1075) <- GROUP1(ACC(),1));
  (code.(1076) <- GROUP2(COMPARE(NEQ())));
  (code.(1077) <- GROUP3(BRANCHIF(true,978)));
  (code.(1078) <- GROUP1(CONST(0),0));
  (code.(1079) <- GROUP1(POP(),2));
  (code.(1080) <- GROUP1(ACC(),6));
  (code.(1081) <- GROUP3(BCOMPARE(NEQ(),0,1084)));
  (code.(1082) <- GROUP1(CONST(1),0));
  (code.(1083) <- GROUP3(BRANCH(1085)));
  (code.(1084) <- GROUP1(CONST(-1),0));
  (code.(1085) <- GROUP1(PUSH(),0));
  (code.(1086) <- GROUP1(ACC(),8));
  (code.(1087) <- GROUP1(PUSH(),0));
  (code.(1088) <- GROUP1(GETGLOBAL(),58));
  (code.(1089) <- (CALL(18,true,false,false,false)));
  (code.(1090) <- GROUP2(MUL()));
  (code.(1091) <- GROUP4(POPTRAP()));
  (code.(1092) <- RETURN(9));
  (code.(1093) <- GROUP1(PUSH(),0));
  (code.(1094) <- GROUP1(GETGLOBAL(),57));
  (code.(1095) <- GROUP1(PUSH(),0));
  (code.(1096) <- GROUP1(ACC(),1));
  (code.(1097) <- GROUP2(COMPARE(EQ())));
  (code.(1098) <- GROUP3(BRANCHIF(true,1106)));
  (code.(1099) <- GROUP1(GETGLOBAL(),56));
  (code.(1100) <- GROUP1(PUSH(),0));
  (code.(1101) <- GROUP1(ACC(),1));
  (code.(1102) <- GROUP2(COMPARE(EQ())));
  (code.(1103) <- GROUP3(BRANCHIF(true,1106)));
  (code.(1104) <- GROUP1(ACC(),0));
  (code.(1105) <- GROUP4(RAISE()));
  (code.(1106) <- GROUP1(CONST(0),0));
  (code.(1107) <- RETURN(10));
  (code.(1108) <- GROUP4(RESTART()));
  (code.(1109) <- GRAB(2));
  (code.(1110) <- GROUP1(ACC(),0));
  (code.(1111) <- GROUP3(BRANCHIF(false,1120)));
  (code.(1112) <- GROUP1(CONST(0),0));
  (code.(1113) <- GROUP1(PUSH(),0));
  (code.(1114) <- GROUP1(ACC(),3));
  (code.(1115) <- GROUP1(PUSH(),0));
  (code.(1116) <- GROUP1(ACC(),3));
  (code.(1117) <- GROUP1(PUSH(),0));
  (code.(1118) <- GROUP1(GETGLOBAL(),59));
  (code.(1119) <- APPTERM(3,6));
  (code.(1120) <- GROUP1(CONST(1),0));
  (code.(1121) <- GROUP1(PUSH(),0));
  (code.(1122) <- GROUP1(ACC(),3));
  (code.(1123) <- GROUP1(PUSH(),0));
  (code.(1124) <- GROUP1(ACC(),3));
  (code.(1125) <- GROUP1(PUSH(),0));
  (code.(1126) <- GROUP1(GETGLOBAL(),59));
  (code.(1127) <- APPTERM(3,6));
  (code.(1128) <- GROUP4(RESTART()));
  (code.(1129) <- GRAB(2));
  (code.(1130) <- GROUP1(ACC(),1));
  (code.(1131) <- GROUP1(PUSH(),0));
  (code.(1132) <- GROUP1(GETGLOBAL(),60));
  (code.(1133) <- GROUP1(PUSH(),0));
  (code.(1134) <- GROUP1(GETGLOBAL(),61));
  (code.(1135) <- APPLY(true,2,0));
  (code.(1136) <- GROUP1(PUSH(),0));
  (code.(1137) <- GROUP1(ACC(),3));
  (code.(1138) <- GROUP1(PUSH(),0));
  (code.(1139) <- GROUP1(ACC(),2));
  (code.(1140) <- GROUP1(OFFSET(-1),0));
  (code.(1141) <- GROUP1(PUSH(),0));
  (code.(1142) <- GROUP1(ACC(),3));
  (code.(1143) <- GROUP1(PUSH(),0));
  (code.(1144) <- GROUP1(ACC(),3));
  (code.(1145) <- GROUP1(PUSH(),0));
  (code.(1146) <- GROUP1(GETGLOBAL(),62));
  (code.(1147) <- APPLY(true,2,0));
  (code.(1148) <- GROUP1(OFFSET(-1),0));
  (code.(1149) <- GROUP1(PUSH(),0));
  (code.(1150) <- GROUP1(ACC(),3));
  (code.(1151) <- (CALL(18,true,false,false,false)));
  (code.(1152) <- (CALL(19,true,true,false,false)));
  (code.(1153) <- GROUP1(ACC(),0));
  (code.(1154) <- RETURN(4));
  (code.(1155) <- GROUP4(RESTART()));
  (code.(1156) <- GRAB(1));
  (code.(1157) <- GROUP1(GETGLOBAL(),63));
  (code.(1158) <- GROUP1(PUSH(),0));
  (code.(1159) <- GROUP3(BRANCH(1164)));
  (code.(1160) <- GROUP3(CHECK_SIGNALS()));
  (code.(1161) <- GROUP1(ACC(),0));
  (code.(1162) <- GROUP1(OFFSET(-1),0));
  (code.(1163) <- GROUP1(ASSIGN(),0));
  (code.(1164) <- GROUP1(ACC(),0));
  (code.(1165) <- GROUP3(BCOMPARE(GE(),0,1176)));
  (code.(1166) <- GROUP1(ACC(),2));
  (code.(1167) <- GROUP1(OFFSET(-1),0));
  (code.(1168) <- GROUP1(PUSH(),0));
  (code.(1169) <- GROUP1(ACC(),1));
  (code.(1170) <- GROUP1(OFFSET(-1),0));
  (code.(1171) <- GROUP1(PUSH(),0));
  (code.(1172) <- GROUP1(ACC(),3));
  (code.(1173) <- (CALL(18,true,false,false,false)));
  (code.(1174) <- (CALL(18,true,false,false,false)));
  (code.(1175) <- GROUP3(BCOMPARE(EQ(),2,1160)));
  (code.(1176) <- GROUP1(ACC(),0));
  (code.(1177) <- GROUP1(OFFSET(1),0));
  (code.(1178) <- RETURN(3));
  (code.(1179) <- GROUP4(RESTART()));
  (code.(1180) <- GRAB(1));
  (code.(1181) <- GROUP1(CONST(0),0));
  (code.(1182) <- GROUP1(PUSH(),0));
  (code.(1183) <- GROUP1(CONST(0),0));
  (code.(1184) <- GROUP1(PUSH(),0));
  (code.(1185) <- GROUP1(GETGLOBAL(),64));
  (code.(1186) <- GROUP1(OFFSET(-1),0));
  (code.(1187) <- GROUP1(PUSH(),0));
  (code.(1188) <- GROUP1(PUSH(),0));
  (code.(1189) <- GROUP1(ACC(),2));
  (code.(1190) <- GROUP2(COMPARE(GT())));
  (code.(1191) <- GROUP3(BRANCHIF(true,1218)));
  (code.(1192) <- GROUP3(CHECK_SIGNALS()));
  (code.(1193) <- GROUP1(ACC(),1));
  (code.(1194) <- GROUP1(PUSH(),0));
  (code.(1195) <- GROUP1(GETGLOBAL(),63));
  (code.(1196) <- GROUP1(OFFSET(-1),0));
  (code.(1197) <- GROUP1(PUSH(),0));
  (code.(1198) <- GROUP1(ACC(),6));
  (code.(1199) <- (CALL(18,true,false,false,false)));
  (code.(1200) <- (CALL(18,true,false,false,false)));
  (code.(1201) <- GROUP1(PUSH(),0));
  (code.(1202) <- GROUP1(CONST(2),0));
  (code.(1203) <- GROUP2(COMPARE(EQ())));
  (code.(1204) <- GROUP3(BRANCHIF(false,1211)));
  (code.(1205) <- GROUP1(ACC(),2));
  (code.(1206) <- GROUP1(PUSH(),0));
  (code.(1207) <- GROUP1(ACC(),2));
  (code.(1208) <- GROUP1(OFFSET(1),0));
  (code.(1209) <- MAKEBLOCK(false,false,2,0,0));
  (code.(1210) <- GROUP1(ASSIGN(),2));
  (code.(1211) <- GROUP1(ACC(),1));
  (code.(1212) <- GROUP1(PUSH(),0));
  (code.(1213) <- GROUP1(OFFSET(1),0));
  (code.(1214) <- GROUP1(ASSIGN(),2));
  (code.(1215) <- GROUP1(ACC(),1));
  (code.(1216) <- GROUP2(COMPARE(NEQ())));
  (code.(1217) <- GROUP3(BRANCHIF(true,1192)));
  (code.(1218) <- GROUP1(CONST(0),0));
  (code.(1219) <- GROUP1(POP(),2));
  (code.(1220) <- GROUP1(ACC(),0));
  (code.(1221) <- RETURN(3));
  (code.(1222) <- GROUP1(CONST(2),0));
  (code.(1223) <- GROUP1(PUSH(),0));
  (code.(1224) <- GROUP1(GETGLOBAL(),64));
  (code.(1225) <- GROUP1(PUSH(),0));
  (code.(1226) <- GROUP1(GETGLOBAL(),63));
  (code.(1227) <- GROUP1(PUSH(),0));
  (code.(1228) <- GROUP1(GETGLOBAL(),65));
  (code.(1229) <- APPTERM(3,4));
  (code.(1230) <- GROUP1(CONST(0),0));
  (code.(1231) <- GROUP1(PUSH(),0));
  (code.(1232) <- GROUP1(ENVACC(),4));
  (code.(1233) <- GROUP1(GETFIELD(),8));
  (code.(1234) <- APPLY(true,1,0));
  (code.(1235) <- GROUP1(PUSH(),0));
  (code.(1236) <- GROUP1(CONST(0),0));
  (code.(1237) <- GROUP1(PUSH(),0));
  (code.(1238) <- GROUP1(ENVACC(),4));
  (code.(1239) <- GROUP1(GETFIELD(),8));
  (code.(1240) <- APPLY(true,1,0));
  (code.(1241) <- GROUP1(PUSH(),0));
  (code.(1242) <- GROUP1(CONST(0),0));
  (code.(1243) <- GROUP1(PUSH(),0));
  (code.(1244) <- GROUP1(ENVACC(),5));
  (code.(1245) <- GROUP1(GETFIELD(),0));
  (code.(1246) <- APPLY(true,1,0));
  (code.(1247) <- GROUP1(PUSH(),0));
  (code.(1248) <- GROUP1(ENVACC(),1));
  (code.(1249) <- GROUP2(SETFIELD(0)));
  (code.(1250) <- GROUP1(CONST(0),0));
  (code.(1251) <- GROUP1(PUSH(),0));
  (code.(1252) <- GROUP1(ENVACC(),4));
  (code.(1253) <- GROUP1(GETFIELD(),5));
  (code.(1254) <- APPLY(true,1,0));
  (code.(1255) <- GROUP1(ACC(),1));
  (code.(1256) <- GROUP3(BRANCHIF(false,1281)));
  (code.(1257) <- GROUP1(ACC(),0));
  (code.(1258) <- GROUP3(BRANCHIF(false,1270)));
  (code.(1259) <- GROUP1(CONST(0),0));
  (code.(1260) <- GROUP1(PUSH(),0));
  (code.(1261) <- GROUP1(ENVACC(),3));
  (code.(1262) <- APPLY(true,1,0));
  (code.(1263) <- GROUP1(PUSH(),0));
  (code.(1264) <- GROUP1(CONST(1),0));
  (code.(1265) <- GROUP1(PUSH(),0));
  (code.(1266) <- GROUP1(ENVACC(),3));
  (code.(1267) <- APPLY(true,1,0));
  (code.(1268) <- MAKEBLOCK(false,false,2,0,0));
  (code.(1269) <- RETURN(3));
  (code.(1270) <- GROUP1(CONST(0),0));
  (code.(1271) <- GROUP1(PUSH(),0));
  (code.(1272) <- GROUP1(ENVACC(),2));
  (code.(1273) <- APPLY(true,1,0));
  (code.(1274) <- GROUP1(PUSH(),0));
  (code.(1275) <- GROUP1(CONST(1),0));
  (code.(1276) <- GROUP1(PUSH(),0));
  (code.(1277) <- GROUP1(ENVACC(),3));
  (code.(1278) <- APPLY(true,1,0));
  (code.(1279) <- MAKEBLOCK(false,false,2,0,0));
  (code.(1280) <- RETURN(3));
  (code.(1281) <- GROUP1(ACC(),0));
  (code.(1282) <- GROUP3(BRANCHIF(false,1294)));
  (code.(1283) <- GROUP1(CONST(0),0));
  (code.(1284) <- GROUP1(PUSH(),0));
  (code.(1285) <- GROUP1(ENVACC(),3));
  (code.(1286) <- APPLY(true,1,0));
  (code.(1287) <- GROUP1(PUSH(),0));
  (code.(1288) <- GROUP1(CONST(1),0));
  (code.(1289) <- GROUP1(PUSH(),0));
  (code.(1290) <- GROUP1(ENVACC(),2));
  (code.(1291) <- APPLY(true,1,0));
  (code.(1292) <- MAKEBLOCK(false,false,2,0,0));
  (code.(1293) <- RETURN(3));
  (code.(1294) <- GROUP1(CONST(0),0));
  (code.(1295) <- GROUP1(PUSH(),0));
  (code.(1296) <- GROUP1(ENVACC(),2));
  (code.(1297) <- APPLY(true,1,0));
  (code.(1298) <- GROUP1(PUSH(),0));
  (code.(1299) <- GROUP1(CONST(1),0));
  (code.(1300) <- GROUP1(PUSH(),0));
  (code.(1301) <- GROUP1(ENVACC(),2));
  (code.(1302) <- APPLY(true,1,0));
  (code.(1303) <- MAKEBLOCK(false,false,2,0,0));
  (code.(1304) <- RETURN(3));
  (code.(1305) <- GROUP4(RESTART()));
  (code.(1306) <- GRAB(1));
  (code.(1307) <- GROUP1(ENVACC(),5));
  (code.(1308) <- GROUP1(GETFIELD(),0));
  (code.(1309) <- GROUP1(PUSH(),0));
  (code.(1310) <- GROUP1(ACC(),1));
  (code.(1311) <- GROUP1(PUSH(),0));
  (code.(1312) <- GROUP1(ENVACC(),4));
  (code.(1313) <- GROUP1(GETFIELD(),0));
  (code.(1314) <- GROUP1(PUSH(),0));
  (code.(1315) <- GROUP1(ENVACC(),6));
  (code.(1316) <- GROUP1(GETFIELD(),0));
  (code.(1317) <- APPLY(true,3,0));
  (code.(1318) <- GROUP1(PUSH(),0));
  (code.(1319) <- GROUP1(ENVACC(),5));
  (code.(1320) <- GROUP1(GETFIELD(),0));
  (code.(1321) <- GROUP1(PUSH(),0));
  (code.(1322) <- GROUP1(ENVACC(),5));
  (code.(1323) <- GROUP1(GETFIELD(),0));
  (code.(1324) <- GROUP1(PUSH(),0));
  (code.(1325) <- GROUP1(ACC(),2));
  (code.(1326) <- GROUP1(PUSH(),0));
  (code.(1327) <- GROUP1(ACC(),4));
  (code.(1328) <- GROUP1(PUSH(),0));
  (code.(1329) <- GROUP1(ENVACC(),9));
  (code.(1330) <- GROUP1(GETFIELD(),2));
  (code.(1331) <- APPLY(true,3,0));
  (code.(1332) <- GROUP1(PUSH(),0));
  (code.(1333) <- GROUP1(ENVACC(),5));
  (code.(1334) <- GROUP2(SETFIELD(0)));
  (code.(1335) <- GROUP1(PUSHRETADDR(),1348));
  (code.(1336) <- GROUP1(ENVACC(),5));
  (code.(1337) <- GROUP1(GETFIELD(),0));
  (code.(1338) <- GROUP1(PUSH(),0));
  (code.(1339) <- GROUP1(ACC(),4));
  (code.(1340) <- GROUP1(PUSH(),0));
  (code.(1341) <- GROUP1(ACC(),6));
  (code.(1342) <- GROUP1(PUSH(),0));
  (code.(1343) <- GROUP1(ACC(),8));
  (code.(1344) <- GROUP1(PUSH(),0));
  (code.(1345) <- GROUP1(ENVACC(),8));
  (code.(1346) <- GROUP1(GETFIELD(),6));
  (code.(1347) <- APPLY(false,4,0));
  (code.(1348) <- GROUP1(ENVACC(),5));
  (code.(1349) <- GROUP1(GETFIELD(),0));
  (code.(1350) <- GROUP1(PUSH(),0));
  (code.(1351) <- GROUP1(ACC(),3));
  (code.(1352) <- GROUP1(PUSH(),0));
  (code.(1353) <- GROUP1(ENVACC(),7));
  (code.(1354) <- GROUP1(GETFIELD(),5));
  (code.(1355) <- APPLY(true,2,0));
  (code.(1356) <- GROUP1(PUSH(),0));
  (code.(1357) <- GROUP1(ACC(),0));
  (code.(1358) <- GROUP3(SWITCH(2574,2578)));
  (code.(1359) <- GROUP1(ENVACC(),1));
  (code.(1360) <- GROUP4(RAISE()));
  (code.(1361) <- GROUP1(ENVACC(),2));
  (code.(1362) <- GROUP4(RAISE()));
  (code.(1363) <- GROUP1(ENVACC(),3));
  (code.(1364) <- GROUP4(RAISE()));
  (code.(1365) <- GROUP1(CONST(0),0));
  (code.(1366) <- RETURN(5));
  (code.(1367) <- GROUP4(RESTART()));
  (code.(1368) <- GRAB(1));
  (code.(1369) <- GROUP1(ENVACC(),4));
  (code.(1370) <- GROUP1(GETFIELD(),0));
  (code.(1371) <- GROUP1(PUSH(),0));
  (code.(1372) <- GROUP1(ACC(),1));
  (code.(1373) <- GROUP1(PUSH(),0));
  (code.(1374) <- GROUP1(ENVACC(),6));
  (code.(1375) <- GROUP1(GETFIELD(),7));
  (code.(1376) <- APPLY(true,2,0));
  (code.(1377) <- GROUP1(PUSH(),0));
  (code.(1378) <- GROUP1(ENVACC(),4));
  (code.(1379) <- GROUP1(GETFIELD(),0));
  (code.(1380) <- GROUP1(PUSH(),0));
  (code.(1381) <- GROUP1(ENVACC(),4));
  (code.(1382) <- GROUP1(GETFIELD(),0));
  (code.(1383) <- GROUP1(PUSH(),0));
  (code.(1384) <- GROUP1(ACC(),2));
  (code.(1385) <- GROUP1(PUSH(),0));
  (code.(1386) <- GROUP1(ACC(),4));
  (code.(1387) <- GROUP1(PUSH(),0));
  (code.(1388) <- GROUP1(ENVACC(),7));
  (code.(1389) <- GROUP1(GETFIELD(),2));
  (code.(1390) <- APPLY(true,3,0));
  (code.(1391) <- GROUP1(PUSH(),0));
  (code.(1392) <- GROUP1(ENVACC(),4));
  (code.(1393) <- GROUP2(SETFIELD(0)));
  (code.(1394) <- GROUP1(PUSHRETADDR(),1407));
  (code.(1395) <- GROUP1(ENVACC(),4));
  (code.(1396) <- GROUP1(GETFIELD(),0));
  (code.(1397) <- GROUP1(PUSH(),0));
  (code.(1398) <- GROUP1(ACC(),4));
  (code.(1399) <- GROUP1(PUSH(),0));
  (code.(1400) <- GROUP1(ACC(),6));
  (code.(1401) <- GROUP1(PUSH(),0));
  (code.(1402) <- GROUP1(ACC(),8));
  (code.(1403) <- GROUP1(PUSH(),0));
  (code.(1404) <- GROUP1(ENVACC(),6));
  (code.(1405) <- GROUP1(GETFIELD(),6));
  (code.(1406) <- APPLY(false,4,0));
  (code.(1407) <- GROUP1(ENVACC(),4));
  (code.(1408) <- GROUP1(GETFIELD(),0));
  (code.(1409) <- GROUP1(PUSH(),0));
  (code.(1410) <- GROUP1(ACC(),3));
  (code.(1411) <- GROUP1(PUSH(),0));
  (code.(1412) <- GROUP1(ENVACC(),5));
  (code.(1413) <- GROUP1(GETFIELD(),5));
  (code.(1414) <- APPLY(true,2,0));
  (code.(1415) <- GROUP1(PUSH(),0));
  (code.(1416) <- GROUP1(ACC(),0));
  (code.(1417) <- GROUP3(SWITCH(2578,2582)));
  (code.(1418) <- GROUP1(ENVACC(),1));
  (code.(1419) <- GROUP4(RAISE()));
  (code.(1420) <- GROUP1(ENVACC(),2));
  (code.(1421) <- GROUP4(RAISE()));
  (code.(1422) <- GROUP1(ENVACC(),3));
  (code.(1423) <- GROUP4(RAISE()));
  (code.(1424) <- GROUP1(CONST(0),0));
  (code.(1425) <- RETURN(5));
  (code.(1426) <- GROUP4(RESTART()));
  (code.(1427) <- GRAB(3));
  (code.(1428) <- GROUP1(CONST(4),0));
  (code.(1429) <- MAKEBLOCK(false,false,1,0,0));
  (code.(1430) <- GROUP1(PUSH(),0));
  (code.(1431) <- GROUP1(CONST(0),0));
  (code.(1432) <- (CALL(10,false,false,false,false)));
  (code.(1433) <- GROUP1(PUSH(),0));
  (code.(1434) <- GROUP1(GETGLOBAL(),66));
  (code.(1435) <- MAKEBLOCK(false,false,2,248,0));
  (code.(1436) <- GROUP1(PUSH(),0));
  (code.(1437) <- GROUP1(CONST(0),0));
  (code.(1438) <- (CALL(10,false,false,false,false)));
  (code.(1439) <- GROUP1(PUSH(),0));
  (code.(1440) <- GROUP1(GETGLOBAL(),67));
  (code.(1441) <- MAKEBLOCK(false,false,2,248,0));
  (code.(1442) <- GROUP1(PUSH(),0));
  (code.(1443) <- GROUP1(CONST(0),0));
  (code.(1444) <- (CALL(10,false,false,false,false)));
  (code.(1445) <- GROUP1(PUSH(),0));
  (code.(1446) <- GROUP1(GETGLOBAL(),68));
  (code.(1447) <- MAKEBLOCK(false,false,2,248,0));
  (code.(1448) <- GROUP1(PUSH(),0));
  (code.(1449) <- GROUP1(ACC(),5));
  (code.(1450) <- GROUP1(GETFIELD(),2));
  (code.(1451) <- GROUP1(PUSH(),0));
  (code.(1452) <- GROUP1(ACC(),6));
  (code.(1453) <- GROUP1(GETFIELD(),3));
  (code.(1454) <- GROUP1(PUSH(),0));
  (code.(1455) <- GROUP1(ACC(),7));
  (code.(1456) <- GROUP1(GETFIELD(),4));
  (code.(1457) <- GROUP1(PUSH(),0));
  (code.(1458) <- GROUP1(ACC(),8));
  (code.(1459) <- GROUP1(GETFIELD(),10));
  (code.(1460) <- GROUP1(PUSH(),0));
  (code.(1461) <- GROUP1(CONST(0),0));
  (code.(1462) <- GROUP1(PUSH(),0));
  (code.(1463) <- GROUP1(ACC(),9));
  (code.(1464) <- GROUP1(GETFIELD(),0));
  (code.(1465) <- APPLY(true,1,0));
  (code.(1466) <- MAKEBLOCK(false,false,1,0,0));
  (code.(1467) <- GROUP1(PUSH(),0));
  (code.(1468) <- GROUP1(ACC(),10));
  (code.(1469) <- GROUP1(GETFIELD(),1));
  (code.(1470) <- GROUP1(PUSH(),0));
  (code.(1471) <- GROUP1(ACC(),11));
  (code.(1472) <- GROUP1(GETFIELD(),0));
  (code.(1473) <- GROUP1(PUSH(),0));
  (code.(1474) <- GROUP1(ACC(),11));
  (code.(1475) <- GROUP1(PUSH(),0));
  (code.(1476) <- GROUP1(ACC(),13));
  (code.(1477) <- GROUP1(PUSH(),0));
  (code.(1478) <- GROUP1(ACC(),15));
  (code.(1479) <- GROUP1(PUSH(),0));
  (code.(1480) <- GROUP1(ACC(),5));
  (code.(1481) <- GROUP1(PUSH(),0));
  (code.(1482) <- GROUP1(ACC(),11));
  (code.(1483) <- GROUP1(PUSH(),0));
  (code.(1484) <- GROUP1(ACC(),13));
  (code.(1485) <- GROUP1(PUSH(),0));
  (code.(1486) <- GROUP1(ACC(),15));
  (code.(1487) <- MAKEBLOCK(false,true,7,closure_tag,1368));
  (code.(1488) <- GROUP1(PUSH(),0));
  (code.(1489) <- GROUP1(ACC(),12));
  (code.(1490) <- GROUP1(PUSH(),0));
  (code.(1491) <- GROUP1(ACC(),14));
  (code.(1492) <- GROUP1(PUSH(),0));
  (code.(1493) <- GROUP1(ACC(),16));
  (code.(1494) <- GROUP1(PUSH(),0));
  (code.(1495) <- GROUP1(ACC(),18));
  (code.(1496) <- GROUP1(PUSH(),0));
  (code.(1497) <- GROUP1(ACC(),7));
  (code.(1498) <- GROUP1(PUSH(),0));
  (code.(1499) <- GROUP1(ACC(),16));
  (code.(1500) <- GROUP1(PUSH(),0));
  (code.(1501) <- GROUP1(ACC(),14));
  (code.(1502) <- GROUP1(PUSH(),0));
  (code.(1503) <- GROUP1(ACC(),16));
  (code.(1504) <- GROUP1(PUSH(),0));
  (code.(1505) <- GROUP1(ACC(),18));
  (code.(1506) <- MAKEBLOCK(false,true,9,closure_tag,1306));
  (code.(1507) <- GROUP1(PUSH(),0));
  (code.(1508) <- GROUP1(ACC(),13));
  (code.(1509) <- GROUP1(PUSH(),0));
  (code.(1510) <- GROUP1(ACC(),15));
  (code.(1511) <- GROUP1(PUSH(),0));
  (code.(1512) <- GROUP1(ACC(),2));
  (code.(1513) <- GROUP1(PUSH(),0));
  (code.(1514) <- GROUP1(ACC(),4));
  (code.(1515) <- GROUP1(PUSH(),0));
  (code.(1516) <- GROUP1(ACC(),8));
  (code.(1517) <- MAKEBLOCK(false,true,5,closure_tag,1230));
  (code.(1518) <- GROUP1(PUSH(),0));
  (code.(1519) <- GROUP1(ACC(),0));
  (code.(1520) <- GROUP1(PUSH(),0));
  (code.(1521) <- GROUP1(ACC(),2));
  (code.(1522) <- GROUP1(PUSH(),0));
  (code.(1523) <- GROUP1(ACC(),4));
  (code.(1524) <- GROUP1(PUSH(),0));
  (code.(1525) <- GROUP1(ACC(),6));
  (code.(1526) <- GROUP1(PUSH(),0));
  (code.(1527) <- GROUP1(ACC(),8));
  (code.(1528) <- GROUP1(PUSH(),0));
  (code.(1529) <- GROUP1(ACC(),10));
  (code.(1530) <- GROUP1(PUSH(),0));
  (code.(1531) <- GROUP1(ACC(),12));
  (code.(1532) <- GROUP1(PUSH(),0));
  (code.(1533) <- GROUP1(ACC(),14));
  (code.(1534) <- GROUP1(PUSH(),0));
  (code.(1535) <- GROUP1(ACC(),16));
  (code.(1536) <- GROUP1(PUSH(),0));
  (code.(1537) <- GROUP1(ACC(),18));
  (code.(1538) <- GROUP1(PUSH(),0));
  (code.(1539) <- GROUP1(ACC(),20));
  (code.(1540) <- GROUP1(PUSH(),0));
  (code.(1541) <- GROUP1(ACC(),22));
  (code.(1542) <- GROUP1(PUSH(),0));
  (code.(1543) <- GROUP1(ACC(),24));
  (code.(1544) <- GROUP1(PUSH(),0));
  (code.(1545) <- GROUP1(ACC(),26));
  (code.(1546) <- MAKEBLOCK(false,false,14,0,0));
  (code.(1547) <- RETURN(18));
  (code.(1548) <- GROUP1(CONST(0),0));
  (code.(1549) <- GROUP1(PUSH(),0));
  (code.(1550) <- GROUP1(CONST(0),0));
  (code.(1551) <- GROUP1(PUSH(),0));
  (code.(1552) <- GROUP1(ENVACC(),2));
  (code.(1553) <- GROUP1(GETFIELD(),0));
  (code.(1554) <- APPLY(true,1,0));
  (code.(1555) <- GROUP3(BRANCH(1615)));
  (code.(1556) <- GROUP3(CHECK_SIGNALS()));
  (code.(1557) <- PUSHTRAP(1568));
  (code.(1558) <- GROUP1(CONST(0),0));
  (code.(1559) <- GROUP1(PUSH(),0));
  (code.(1560) <- GROUP1(ENVACC(),2));
  (code.(1561) <- GROUP1(GETFIELD(),1));
  (code.(1562) <- APPLY(true,1,0));
  (code.(1563) <- GROUP1(PUSH(),0));
  (code.(1564) <- GROUP1(ENVACC(),1));
  (code.(1565) <- APPLY(true,1,0));
  (code.(1566) <- GROUP4(POPTRAP()));
  (code.(1567) <- GROUP3(BRANCH(1608)));
  (code.(1568) <- GROUP1(PUSH(),0));
  (code.(1569) <- GROUP1(ENVACC(),2));
  (code.(1570) <- GROUP1(GETFIELD(),4));
  (code.(1571) <- GROUP1(PUSH(),0));
  (code.(1572) <- GROUP1(ACC(),1));
  (code.(1573) <- GROUP2(COMPARE(EQ())));
  (code.(1574) <- GROUP3(BRANCHIF(false,1581)));
  (code.(1575) <- GROUP1(CONST(0),0));
  (code.(1576) <- GROUP1(PUSH(),0));
  (code.(1577) <- GROUP1(ENVACC(),2));
  (code.(1578) <- GROUP1(GETFIELD(),7));
  (code.(1579) <- APPLY(true,1,0));
  (code.(1580) <- GROUP3(BRANCH(1607)));
  (code.(1581) <- GROUP1(ENVACC(),2));
  (code.(1582) <- GROUP1(GETFIELD(),5));
  (code.(1583) <- GROUP1(PUSH(),0));
  (code.(1584) <- GROUP1(ACC(),1));
  (code.(1585) <- GROUP2(COMPARE(EQ())));
  (code.(1586) <- GROUP3(BRANCHIF(false,1593)));
  (code.(1587) <- GROUP1(CONST(0),0));
  (code.(1588) <- GROUP1(PUSH(),0));
  (code.(1589) <- GROUP1(ENVACC(),2));
  (code.(1590) <- GROUP1(GETFIELD(),8));
  (code.(1591) <- APPLY(true,1,0));
  (code.(1592) <- GROUP3(BRANCH(1607)));
  (code.(1593) <- GROUP1(ENVACC(),2));
  (code.(1594) <- GROUP1(GETFIELD(),6));
  (code.(1595) <- GROUP1(PUSH(),0));
  (code.(1596) <- GROUP1(ACC(),1));
  (code.(1597) <- GROUP2(COMPARE(EQ())));
  (code.(1598) <- GROUP3(BRANCHIF(false,1605)));
  (code.(1599) <- GROUP1(CONST(0),0));
  (code.(1600) <- GROUP1(PUSH(),0));
  (code.(1601) <- GROUP1(ENVACC(),2));
  (code.(1602) <- GROUP1(GETFIELD(),9));
  (code.(1603) <- APPLY(true,1,0));
  (code.(1604) <- GROUP3(BRANCH(1607)));
  (code.(1605) <- GROUP1(ACC(),0));
  (code.(1606) <- GROUP4(RAISE()));
  (code.(1607) <- GROUP1(POP(),1));
  (code.(1608) <- GROUP1(CONST(0),0));
  (code.(1609) <- GROUP1(PUSH(),0));
  (code.(1610) <- GROUP1(ENVACC(),2));
  (code.(1611) <- GROUP1(GETFIELD(),2));
  (code.(1612) <- APPLY(true,1,0));
  (code.(1613) <- GROUP1(UNOP(NOT()),0));
  (code.(1614) <- GROUP1(ASSIGN(),0));
  (code.(1615) <- GROUP1(ACC(),0));
  (code.(1616) <- GROUP3(BRANCHIF(false,1556)));
  (code.(1617) <- GROUP1(CONST(6666),0));
  (code.(1618) <- (CALL(2,false,false,false,false)));
  (code.(1619) <- GROUP1(CONST(0),0));
  (code.(1620) <- GROUP1(PUSH(),0));
  (code.(1621) <- GROUP1(ENVACC(),2));
  (code.(1622) <- GROUP1(GETFIELD(),3));
  (code.(1623) <- APPTERM(1,3));
  (code.(1624) <- GROUP3(BRANCH(1636)));
  (code.(1625) <- GROUP3(CHECK_SIGNALS()));
  (code.(1626) <- GROUP1(CONST(0),0));
  (code.(1627) <- GROUP1(PUSH(),0));
  (code.(1628) <- GROUP1(ACC(),1));
  (code.(1629) <- GROUP1(GETFIELD(),0));
  (code.(1630) <- APPLY(true,1,0));
  (code.(1631) <- GROUP1(CONST(0),0));
  (code.(1632) <- GROUP1(PUSH(),0));
  (code.(1633) <- GROUP1(ACC(),1));
  (code.(1634) <- GROUP1(GETFIELD(),1));
  (code.(1635) <- APPLY(true,1,0));
  (code.(1636) <- GROUP1(CONST(1),0));
  (code.(1637) <- GROUP3(BRANCHIF(true,1625)));
  (code.(1638) <- GROUP1(CONST(0),0));
  (code.(1639) <- RETURN(1));
  (code.(1640) <- MAKEBLOCK(false,true,0,closure_tag,1624));
  (code.(1641) <- GROUP1(PUSH(),0));
  (code.(1642) <- GROUP1(ACC(),1));
  (code.(1643) <- GROUP1(PUSH(),0));
  (code.(1644) <- GROUP1(ACC(),1));
  (code.(1645) <- MAKEBLOCK(false,true,2,closure_tag,1548));
  (code.(1646) <- GROUP1(PUSH(),0));
  (code.(1647) <- GROUP1(ACC(),0));
  (code.(1648) <- GROUP1(PUSH(),0));
  (code.(1649) <- GROUP1(ACC(),2));
  (code.(1650) <- MAKEBLOCK(false,false,2,0,0));
  (code.(1651) <- RETURN(3));
  (code.(1652) <- GROUP4(RESTART()));
  (code.(1653) <- GRAB(1));
  (code.(1654) <- GROUP1(ACC(),1));
  (code.(1655) <- GROUP1(PUSH(),0));
  (code.(1656) <- GROUP1(ACC(),1));
  (code.(1657) <- GROUP1(PUSH(),0));
  (code.(1658) <- GROUP1(GETGLOBAL(),69));
  (code.(1659) <- APPLY(true,2,0));
  (code.(1660) <- GROUP1(PUSH(),0));
  (code.(1661) <- GROUP1(ACC(),0));
  (code.(1662) <- GROUP1(GETFIELD(),7));
  (code.(1663) <- MAKEBLOCK(false,false,1,0,0));
  (code.(1664) <- RETURN(3));
  (code.(1665) <- GROUP4(RESTART()));
  (code.(1666) <- GRAB(3));
  (code.(1667) <- GROUP1(ACC(),0));
  (code.(1668) <- GROUP3(BCOMPARE(LE(),1,1677)));
  (code.(1669) <- GROUP1(ACC(),1));
  (code.(1670) <- GROUP1(PUSH(),0));
  (code.(1671) <- GROUP1(CONST(1),0));
  (code.(1672) <- GROUP1(PUSH(),0));
  (code.(1673) <- GROUP1(ENVACC(),7));
  (code.(1674) <- GROUP1(GETFIELD(),4));
  (code.(1675) <- APPLY(true,2,0));
  (code.(1676) <- GROUP3(BRANCHIF(true,1685)));
  (code.(1677) <- GROUP1(ACC(),1));
  (code.(1678) <- GROUP1(PUSH(),0));
  (code.(1679) <- GROUP1(CONST(1),0));
  (code.(1680) <- GROUP1(PUSH(),0));
  (code.(1681) <- GROUP1(ENVACC(),7));
  (code.(1682) <- GROUP1(GETFIELD(),3));
  (code.(1683) <- APPLY(true,2,0));
  (code.(1684) <- GROUP3(BRANCHIF(false,1692)));
  (code.(1685) <- GROUP1(ACC(),1));
  (code.(1686) <- GROUP1(PUSH(),0));
  (code.(1687) <- GROUP1(CONST(1),0));
  (code.(1688) <- GROUP1(PUSH(),0));
  (code.(1689) <- GROUP1(ENVACC(),7));
  (code.(1690) <- GROUP1(GETFIELD(),0));
  (code.(1691) <- APPTERM(2,6));
  (code.(1692) <- PUSHTRAP(1723));
  (code.(1693) <- GROUP1(ACC(),7));
  (code.(1694) <- GROUP1(PUSH(),0));
  (code.(1695) <- GROUP1(ACC(),5));
  (code.(1696) <- GROUP1(OFFSET(-1),0));
  (code.(1697) <- GROUP1(PUSH(),0));
  (code.(1698) <- GROUP1(OFFSETCLOSURE(),1));
  (code.(1699) <- APPLY(true,1,0));
  (code.(1700) <- GROUP1(PUSH(),0));
  (code.(1701) <- GROUP1(ACC(),7));
  (code.(1702) <- GROUP1(PUSH(),0));
  (code.(1703) <- GROUP1(ENVACC(),5));
  (code.(1704) <- APPLY(true,3,0));
  (code.(1705) <- GROUP1(PUSH(),0));
  (code.(1706) <- GROUP1(ACC(),6));
  (code.(1707) <- GROUP1(PUSH(),0));
  (code.(1708) <- GROUP1(CONST(1),0));
  (code.(1709) <- GROUP1(PUSH(),0));
  (code.(1710) <- GROUP1(ENVACC(),8));
  (code.(1711) <- GROUP1(GETFIELD(),1));
  (code.(1712) <- APPLY(true,2,0));
  (code.(1713) <- GROUP1(PUSH(),0));
  (code.(1714) <- GROUP1(ACC(),8));
  (code.(1715) <- GROUP1(PUSH(),0));
  (code.(1716) <- GROUP1(ACC(),2));
  (code.(1717) <- GROUP1(PUSH(),0));
  (code.(1718) <- GROUP1(GETGLOBAL(),70));
  (code.(1719) <- APPLY(true,3,0));
  (code.(1720) <- GROUP1(POP(),1));
  (code.(1721) <- GROUP4(POPTRAP()));
  (code.(1722) <- RETURN(4));
  (code.(1723) <- GROUP1(PUSH(),0));
  (code.(1724) <- GROUP1(ENVACC(),4));
  (code.(1725) <- GROUP1(PUSH(),0));
  (code.(1726) <- GROUP1(ACC(),1));
  (code.(1727) <- GROUP1(GETFIELD(),0));
  (code.(1728) <- GROUP2(COMPARE(EQ())));
  (code.(1729) <- GROUP3(BRANCHIF(false,1733)));
  (code.(1730) <- GROUP1(ACC(),0));
  (code.(1731) <- GROUP1(GETFIELD(),1));
  (code.(1732) <- RETURN(5));
  (code.(1733) <- GROUP1(ACC(),0));
  (code.(1734) <- GROUP4(RAISE()));
  (code.(1735) <- GROUP4(RESTART()));
  (code.(1736) <- GRAB(3));
  (code.(1737) <- GROUP1(ACC(),0));
  (code.(1738) <- GROUP3(BCOMPARE(LE(),1,1747)));
  (code.(1739) <- GROUP1(ACC(),1));
  (code.(1740) <- GROUP1(PUSH(),0));
  (code.(1741) <- GROUP1(CONST(0),0));
  (code.(1742) <- GROUP1(PUSH(),0));
  (code.(1743) <- GROUP1(ENVACC(),5));
  (code.(1744) <- GROUP1(GETFIELD(),4));
  (code.(1745) <- APPLY(true,2,0));
  (code.(1746) <- GROUP3(BRANCHIF(true,1755)));
  (code.(1747) <- GROUP1(ACC(),1));
  (code.(1748) <- GROUP1(PUSH(),0));
  (code.(1749) <- GROUP1(CONST(0),0));
  (code.(1750) <- GROUP1(PUSH(),0));
  (code.(1751) <- GROUP1(ENVACC(),5));
  (code.(1752) <- GROUP1(GETFIELD(),3));
  (code.(1753) <- APPLY(true,2,0));
  (code.(1754) <- GROUP3(BRANCHIF(false,1762)));
  (code.(1755) <- GROUP1(ACC(),1));
  (code.(1756) <- GROUP1(PUSH(),0));
  (code.(1757) <- GROUP1(CONST(0),0));
  (code.(1758) <- GROUP1(PUSH(),0));
  (code.(1759) <- GROUP1(ENVACC(),5));
  (code.(1760) <- GROUP1(GETFIELD(),0));
  (code.(1761) <- APPTERM(2,6));
  (code.(1762) <- PUSHTRAP(1793));
  (code.(1763) <- GROUP1(ACC(),7));
  (code.(1764) <- GROUP1(PUSH(),0));
  (code.(1765) <- GROUP1(ACC(),5));
  (code.(1766) <- GROUP1(OFFSET(-1),0));
  (code.(1767) <- GROUP1(PUSH(),0));
  (code.(1768) <- GROUP1(OFFSETCLOSURE(),-1));
  (code.(1769) <- APPLY(true,1,0));
  (code.(1770) <- GROUP1(PUSH(),0));
  (code.(1771) <- GROUP1(ACC(),7));
  (code.(1772) <- GROUP1(PUSH(),0));
  (code.(1773) <- GROUP1(ENVACC(),4));
  (code.(1774) <- APPLY(true,3,0));
  (code.(1775) <- GROUP1(PUSH(),0));
  (code.(1776) <- GROUP1(ACC(),6));
  (code.(1777) <- GROUP1(PUSH(),0));
  (code.(1778) <- GROUP1(CONST(0),0));
  (code.(1779) <- GROUP1(PUSH(),0));
  (code.(1780) <- GROUP1(ENVACC(),6));
  (code.(1781) <- GROUP1(GETFIELD(),1));
  (code.(1782) <- APPLY(true,2,0));
  (code.(1783) <- GROUP1(PUSH(),0));
  (code.(1784) <- GROUP1(ACC(),8));
  (code.(1785) <- GROUP1(PUSH(),0));
  (code.(1786) <- GROUP1(ACC(),2));
  (code.(1787) <- GROUP1(PUSH(),0));
  (code.(1788) <- GROUP1(GETGLOBAL(),70));
  (code.(1789) <- APPLY(true,3,0));
  (code.(1790) <- GROUP1(POP(),1));
  (code.(1791) <- GROUP4(POPTRAP()));
  (code.(1792) <- RETURN(4));
  (code.(1793) <- GROUP1(PUSH(),0));
  (code.(1794) <- GROUP1(ENVACC(),1));
  (code.(1795) <- GROUP1(PUSH(),0));
  (code.(1796) <- GROUP1(ACC(),1));
  (code.(1797) <- GROUP1(GETFIELD(),0));
  (code.(1798) <- GROUP2(COMPARE(EQ())));
  (code.(1799) <- GROUP3(BRANCHIF(false,1803)));
  (code.(1800) <- GROUP1(ACC(),0));
  (code.(1801) <- GROUP1(GETFIELD(),1));
  (code.(1802) <- RETURN(5));
  (code.(1803) <- GROUP1(ACC(),0));
  (code.(1804) <- GROUP4(RAISE()));
  (code.(1805) <- GROUP4(RESTART()));
  (code.(1806) <- GRAB(2));
  (code.(1807) <- GROUP1(ACC(),1));
  (code.(1808) <- GROUP3(BRANCHIF(false,1830)));
  (code.(1809) <- GROUP1(ACC(),2));
  (code.(1810) <- GROUP3(BRANCHIF(false,1836)));
  (code.(1811) <- GROUP1(ACC(),1));
  (code.(1812) <- GROUP1(GETFIELD(),0));
  (code.(1813) <- GROUP1(PUSH(),0));
  (code.(1814) <- GROUP1(ACC(),1));
  (code.(1815) <- (CALL(5,true,false,false,false)));
  (code.(1816) <- GROUP3(BRANCHIF(false,1820)));
  (code.(1817) <- GROUP1(ACC(),2));
  (code.(1818) <- GROUP1(GETFIELD(),0));
  (code.(1819) <- RETURN(3));
  (code.(1820) <- GROUP1(ACC(),2));
  (code.(1821) <- GROUP1(GETFIELD(),1));
  (code.(1822) <- GROUP1(PUSH(),0));
  (code.(1823) <- GROUP1(ACC(),2));
  (code.(1824) <- GROUP1(GETFIELD(),1));
  (code.(1825) <- GROUP1(PUSH(),0));
  (code.(1826) <- GROUP1(ACC(),2));
  (code.(1827) <- GROUP1(PUSH(),0));
  (code.(1828) <- GROUP1(OFFSETCLOSURE(),0));
  (code.(1829) <- APPTERM(3,6));
  (code.(1830) <- GROUP1(ACC(),2));
  (code.(1831) <- GROUP3(BRANCHIF(true,1836)));
  (code.(1832) <- GROUP1(GETGLOBAL(),71));
  (code.(1833) <- GROUP1(PUSH(),0));
  (code.(1834) <- GROUP1(GETGLOBAL(),14));
  (code.(1835) <- APPTERM(1,4));
  (code.(1836) <- GROUP1(GETGLOBAL(),72));
  (code.(1837) <- GROUP1(PUSH(),0));
  (code.(1838) <- GROUP1(GETGLOBAL(),14));
  (code.(1839) <- APPTERM(1,4));
  (code.(1840) <- GROUP4(RESTART()));
  (code.(1841) <- GRAB(2));
  (code.(1842) <- GROUP1(ENVACC(),6));
  (code.(1843) <- GROUP1(GETFIELD(),2));
  (code.(1844) <- GROUP1(PUSH(),0));
  (code.(1845) <- GROUP1(ENVACC(),6));
  (code.(1846) <- GROUP1(GETFIELD(),1));
  (code.(1847) <- GROUP1(PUSH(),0));
  (code.(1848) <- GROUP1(CONST(0),0));
  (code.(1849) <- GROUP1(PUSH(),0));
  (code.(1850) <- GROUP1(ACC(),5));
  (code.(1851) <- GROUP1(PUSH(),0));
  (code.(1852) <- GROUP1(ACC(),5));
  (code.(1853) <- GROUP1(PUSH(),0));
  (code.(1854) <- GROUP1(ENVACC(),7));
  (code.(1855) <- GROUP1(GETFIELD(),1));
  (code.(1856) <- APPLY(true,2,0));
  (code.(1857) <- GROUP1(PUSH(),0));
  (code.(1858) <- PUSHTRAP(1993));
  (code.(1859) <- GROUP1(CONST(0),0));
  (code.(1860) <- GROUP1(PUSH(),0));
  (code.(1861) <- GROUP1(ACC(),5));
  (code.(1862) <- GROUP1(PUSH(),0));
  (code.(1863) <- GROUP1(GETGLOBAL(),73));
  (code.(1864) <- APPLY(true,1,0));
  (code.(1865) <- GROUP1(OFFSET(-1),0));
  (code.(1866) <- GROUP1(PUSH(),0));
  (code.(1867) <- GROUP1(PUSH(),0));
  (code.(1868) <- GROUP1(ACC(),2));
  (code.(1869) <- GROUP2(COMPARE(GT())));
  (code.(1870) <- GROUP3(BRANCHIF(true,1984)));
  (code.(1871) <- GROUP3(CHECK_SIGNALS()));
  (code.(1872) <- GROUP1(ACC(),11));
  (code.(1873) <- GROUP3(BRANCHIF(false,1926)));
  (code.(1874) <- GROUP1(ACC(),12));
  (code.(1875) <- GROUP1(PUSH(),0));
  (code.(1876) <- GROUP1(ACC(),2));
  (code.(1877) <- GROUP1(PUSH(),0));
  (code.(1878) <- GROUP1(ACC(),8));
  (code.(1879) <- GROUP1(PUSH(),0));
  (code.(1880) <- GROUP1(GETGLOBAL(),74));
  (code.(1881) <- APPLY(true,2,0));
  (code.(1882) <- GROUP1(PUSH(),0));
  (code.(1883) <- GROUP1(ACC(),13));
  (code.(1884) <- GROUP1(PUSH(),0));
  (code.(1885) <- GROUP1(ENVACC(),7));
  (code.(1886) <- GROUP1(GETFIELD(),2));
  (code.(1887) <- APPLY(true,3,0));
  (code.(1888) <- GROUP1(PUSH(),0));
  (code.(1889) <- GROUP1(PUSHRETADDR(),1901));
  (code.(1890) <- GROUP1(ACC(),13));
  (code.(1891) <- GROUP1(PUSH(),0));
  (code.(1892) <- GROUP1(ACC(),13));
  (code.(1893) <- GROUP1(PUSH(),0));
  (code.(1894) <- GROUP1(ACC(),5));
  (code.(1895) <- GROUP1(PUSH(),0));
  (code.(1896) <- GROUP1(ACC(),17));
  (code.(1897) <- GROUP1(OFFSET(-1),0));
  (code.(1898) <- GROUP1(PUSH(),0));
  (code.(1899) <- GROUP1(ENVACC(),4));
  (code.(1900) <- APPLY(false,4,0));
  (code.(1901) <- GROUP1(PUSH(),0));
  (code.(1902) <- GROUP1(ACC(),9));
  (code.(1903) <- GROUP1(PUSH(),0));
  (code.(1904) <- GROUP1(ACC(),1));
  (code.(1905) <- MAKEBLOCK(false,false,2,0,0));
  (code.(1906) <- GROUP1(ASSIGN(),9));
  (code.(1907) <- GROUP1(ACC(),0));
  (code.(1908) <- GROUP1(PUSH(),0));
  (code.(1909) <- GROUP1(ACC(),12));
  (code.(1910) <- GROUP1(PUSH(),0));
  (code.(1911) <- GROUP1(GETGLOBAL(),75));
  (code.(1912) <- APPLY(true,2,0));
  (code.(1913) <- GROUP1(ASSIGN(),11));
  (code.(1914) <- GROUP1(ACC(),10));
  (code.(1915) <- GROUP1(PUSH(),0));
  (code.(1916) <- GROUP1(ACC(),12));
  (code.(1917) <- GROUP2(COMPARE(GE())));
  (code.(1918) <- GROUP3(BRANCHIF(false,1924)));
  (code.(1919) <- GROUP1(ACC(),11));
  (code.(1920) <- GROUP1(PUSH(),0));
  (code.(1921) <- GROUP1(ENVACC(),2));
  (code.(1922) <- MAKEBLOCK(false,false,2,0,0));
  (code.(1923) <- GROUP4(RAISE()));
  (code.(1924) <- GROUP1(POP(),2));
  (code.(1925) <- GROUP3(BRANCH(1977)));
  (code.(1926) <- GROUP1(ACC(),12));
  (code.(1927) <- GROUP1(PUSH(),0));
  (code.(1928) <- GROUP1(ACC(),2));
  (code.(1929) <- GROUP1(PUSH(),0));
  (code.(1930) <- GROUP1(ACC(),8));
  (code.(1931) <- GROUP1(PUSH(),0));
  (code.(1932) <- GROUP1(GETGLOBAL(),74));
  (code.(1933) <- APPLY(true,2,0));
  (code.(1934) <- GROUP1(PUSH(),0));
  (code.(1935) <- GROUP1(ACC(),13));
  (code.(1936) <- GROUP1(PUSH(),0));
  (code.(1937) <- GROUP1(ENVACC(),7));
  (code.(1938) <- GROUP1(GETFIELD(),2));
  (code.(1939) <- APPLY(true,3,0));
  (code.(1940) <- GROUP1(PUSH(),0));
  (code.(1941) <- GROUP1(PUSHRETADDR(),1953));
  (code.(1942) <- GROUP1(ACC(),12));
  (code.(1943) <- GROUP1(PUSH(),0));
  (code.(1944) <- GROUP1(ACC(),14));
  (code.(1945) <- GROUP1(PUSH(),0));
  (code.(1946) <- GROUP1(ACC(),5));
  (code.(1947) <- GROUP1(PUSH(),0));
  (code.(1948) <- GROUP1(ACC(),17));
  (code.(1949) <- GROUP1(OFFSET(-1),0));
  (code.(1950) <- GROUP1(PUSH(),0));
  (code.(1951) <- GROUP1(ENVACC(),3));
  (code.(1952) <- APPLY(false,4,0));
  (code.(1953) <- GROUP1(PUSH(),0));
  (code.(1954) <- GROUP1(ACC(),9));
  (code.(1955) <- GROUP1(PUSH(),0));
  (code.(1956) <- GROUP1(ACC(),1));
  (code.(1957) <- MAKEBLOCK(false,false,2,0,0));
  (code.(1958) <- GROUP1(ASSIGN(),9));
  (code.(1959) <- GROUP1(ACC(),0));
  (code.(1960) <- GROUP1(PUSH(),0));
  (code.(1961) <- GROUP1(ACC(),11));
  (code.(1962) <- GROUP1(PUSH(),0));
  (code.(1963) <- GROUP1(GETGLOBAL(),76));
  (code.(1964) <- APPLY(true,2,0));
  (code.(1965) <- GROUP1(ASSIGN(),10));
  (code.(1966) <- GROUP1(ACC(),11));
  (code.(1967) <- GROUP1(PUSH(),0));
  (code.(1968) <- GROUP1(ACC(),11));
  (code.(1969) <- GROUP2(COMPARE(LE())));
  (code.(1970) <- GROUP3(BRANCHIF(false,1976)));
  (code.(1971) <- GROUP1(ACC(),10));
  (code.(1972) <- GROUP1(PUSH(),0));
  (code.(1973) <- GROUP1(ENVACC(),1));
  (code.(1974) <- MAKEBLOCK(false,false,2,0,0));
  (code.(1975) <- GROUP4(RAISE()));
  (code.(1976) <- GROUP1(POP(),2));
  (code.(1977) <- GROUP1(ACC(),1));
  (code.(1978) <- GROUP1(PUSH(),0));
  (code.(1979) <- GROUP1(OFFSET(1),0));
  (code.(1980) <- GROUP1(ASSIGN(),2));
  (code.(1981) <- GROUP1(ACC(),1));
  (code.(1982) <- GROUP2(COMPARE(NEQ())));
  (code.(1983) <- GROUP3(BRANCHIF(true,1871)));
  (code.(1984) <- GROUP1(CONST(0),0));
  (code.(1985) <- GROUP1(POP(),2));
  (code.(1986) <- GROUP1(ACC(),9));
  (code.(1987) <- GROUP3(BRANCHIF(false,1990)));
  (code.(1988) <- GROUP1(ACC(),7));
  (code.(1989) <- GROUP3(BRANCH(1991)));
  (code.(1990) <- GROUP1(ACC(),6));
  (code.(1991) <- GROUP4(POPTRAP()));
  (code.(1992) <- GROUP3(BRANCH(2015)));
  (code.(1993) <- GROUP1(PUSH(),0));
  (code.(1994) <- GROUP1(ENVACC(),2));
  (code.(1995) <- GROUP1(PUSH(),0));
  (code.(1996) <- GROUP1(ACC(),1));
  (code.(1997) <- GROUP1(GETFIELD(),0));
  (code.(1998) <- GROUP2(COMPARE(EQ())));
  (code.(1999) <- GROUP3(BRANCHIF(false,2003)));
  (code.(2000) <- GROUP1(ACC(),0));
  (code.(2001) <- GROUP1(GETFIELD(),1));
  (code.(2002) <- GROUP3(BRANCH(2014)));
  (code.(2003) <- GROUP1(ENVACC(),1));
  (code.(2004) <- GROUP1(PUSH(),0));
  (code.(2005) <- GROUP1(ACC(),1));
  (code.(2006) <- GROUP1(GETFIELD(),0));
  (code.(2007) <- GROUP2(COMPARE(EQ())));
  (code.(2008) <- GROUP3(BRANCHIF(false,2012)));
  (code.(2009) <- GROUP1(ACC(),0));
  (code.(2010) <- GROUP1(GETFIELD(),1));
  (code.(2011) <- GROUP3(BRANCH(2014)));
  (code.(2012) <- GROUP1(ACC(),0));
  (code.(2013) <- GROUP4(RAISE()));
  (code.(2014) <- GROUP1(POP(),1));
  (code.(2015) <- GROUP1(PUSH(),0));
  (code.(2016) <- GROUP1(ACC(),2));
  (code.(2017) <- GROUP1(PUSH(),0));
  (code.(2018) <- GROUP1(GETGLOBAL(),77));
  (code.(2019) <- APPLY(true,1,0));
  (code.(2020) <- GROUP1(ASSIGN(),2));
  (code.(2021) <- GROUP1(ACC(),1));
  (code.(2022) <- GROUP1(PUSH(),0));
  (code.(2023) <- GROUP1(ACC(),3));
  (code.(2024) <- GROUP1(PUSH(),0));
  (code.(2025) <- GROUP1(ACC(),2));
  (code.(2026) <- GROUP1(PUSH(),0));
  (code.(2027) <- GROUP1(ENVACC(),5));
  (code.(2028) <- APPTERM(3,11));
  (code.(2029) <- GROUP4(RESTART()));
  (code.(2030) <- GRAB(4));
  (code.(2031) <- GROUP1(ACC(),3));
  (code.(2032) <- GROUP1(PUSH(),0));
  (code.(2033) <- GROUP1(ACC(),3));
  (code.(2034) <- GROUP1(PUSH(),0));
  (code.(2035) <- GROUP1(ACC(),2));
  (code.(2036) <- GROUP1(PUSH(),0));
  (code.(2037) <- GROUP1(ACC(),7));
  (code.(2038) <- GROUP1(PUSH(),0));
  (code.(2039) <- GROUP1(CONST(0),0));
  (code.(2040) <- GROUP1(PUSH(),0));
  (code.(2041) <- GROUP1(ENVACC(),2));
  (code.(2042) <- GROUP1(GETFIELD(),2));
  (code.(2043) <- APPLY(true,3,0));
  (code.(2044) <- GROUP1(PUSH(),0));
  (code.(2045) <- GROUP1(ACC(),4));
  (code.(2046) <- APPLY(true,3,0));
  (code.(2047) <- GROUP1(PUSH(),0));
  (code.(2048) <- GROUP1(ACC(),4));
  (code.(2049) <- GROUP1(PUSH(),0));
  (code.(2050) <- GROUP1(GETGLOBAL(),76));
  (code.(2051) <- APPLY(true,2,0));
  (code.(2052) <- GROUP1(PUSH(),0));
  (code.(2053) <- GROUP1(ACC(),3));
  (code.(2054) <- GROUP1(PUSH(),0));
  (code.(2055) <- GROUP1(ACC(),1));
  (code.(2056) <- GROUP2(COMPARE(LE())));
  (code.(2057) <- GROUP3(BRANCHIF(false,2063)));
  (code.(2058) <- GROUP1(ACC(),0));
  (code.(2059) <- GROUP1(PUSH(),0));
  (code.(2060) <- GROUP1(ENVACC(),1));
  (code.(2061) <- MAKEBLOCK(false,false,2,0,0));
  (code.(2062) <- GROUP4(RAISE()));
  (code.(2063) <- GROUP1(ACC(),0));
  (code.(2064) <- RETURN(6));
  (code.(2065) <- GROUP4(RESTART()));
  (code.(2066) <- GRAB(4));
  (code.(2067) <- GROUP1(ACC(),3));
  (code.(2068) <- GROUP1(PUSH(),0));
  (code.(2069) <- GROUP1(ACC(),3));
  (code.(2070) <- GROUP1(PUSH(),0));
  (code.(2071) <- GROUP1(ACC(),2));
  (code.(2072) <- GROUP1(PUSH(),0));
  (code.(2073) <- GROUP1(ACC(),7));
  (code.(2074) <- GROUP1(PUSH(),0));
  (code.(2075) <- GROUP1(CONST(1),0));
  (code.(2076) <- GROUP1(PUSH(),0));
  (code.(2077) <- GROUP1(ENVACC(),2));
  (code.(2078) <- GROUP1(GETFIELD(),2));
  (code.(2079) <- APPLY(true,3,0));
  (code.(2080) <- GROUP1(PUSH(),0));
  (code.(2081) <- GROUP1(ACC(),4));
  (code.(2082) <- APPLY(true,3,0));
  (code.(2083) <- GROUP1(PUSH(),0));
  (code.(2084) <- GROUP1(ACC(),4));
  (code.(2085) <- GROUP1(PUSH(),0));
  (code.(2086) <- GROUP1(GETGLOBAL(),75));
  (code.(2087) <- APPLY(true,2,0));
  (code.(2088) <- GROUP1(PUSH(),0));
  (code.(2089) <- GROUP1(ACC(),3));
  (code.(2090) <- GROUP1(PUSH(),0));
  (code.(2091) <- GROUP1(ACC(),1));
  (code.(2092) <- GROUP2(COMPARE(GE())));
  (code.(2093) <- GROUP3(BRANCHIF(false,2099)));
  (code.(2094) <- GROUP1(ACC(),0));
  (code.(2095) <- GROUP1(PUSH(),0));
  (code.(2096) <- GROUP1(ENVACC(),1));
  (code.(2097) <- MAKEBLOCK(false,false,2,0,0));
  (code.(2098) <- GROUP4(RAISE()));
  (code.(2099) <- GROUP1(ACC(),0));
  (code.(2100) <- RETURN(6));
  (code.(2101) <- GROUP4(RESTART()));
  (code.(2102) <- GRAB(1));
  (code.(2103) <- GROUP1(CONST(0),0));
  (code.(2104) <- (CALL(10,false,false,false,false)));
  (code.(2105) <- GROUP1(PUSH(),0));
  (code.(2106) <- GROUP1(GETGLOBAL(),78));
  (code.(2107) <- MAKEBLOCK(false,false,2,248,0));
  (code.(2108) <- GROUP1(PUSH(),0));
  (code.(2109) <- GROUP1(CONST(0),0));
  (code.(2110) <- (CALL(10,false,false,false,false)));
  (code.(2111) <- GROUP1(PUSH(),0));
  (code.(2112) <- GROUP1(GETGLOBAL(),79));
  (code.(2113) <- MAKEBLOCK(false,false,2,248,0));
  (code.(2114) <- GROUP1(PUSH(),0));
  (code.(2115) <- GROUP1(ACC(),2));
  (code.(2116) <- GROUP1(PUSH(),0));
  (code.(2117) <- GROUP1(ACC(),1));
  (code.(2118) <- MAKEBLOCK(false,true,2,closure_tag,2066));
  (code.(2119) <- GROUP1(PUSH(),0));
  (code.(2120) <- GROUP1(ACC(),3));
  (code.(2121) <- GROUP1(PUSH(),0));
  (code.(2122) <- GROUP1(ACC(),3));
  (code.(2123) <- MAKEBLOCK(false,true,2,closure_tag,2030));
  (code.(2124) <- GROUP1(PUSH(),0));
  (code.(2125) <- GROUP1(ACC(),4));
  (code.(2126) <- GROUP1(PUSH(),0));
  (code.(2127) <- GROUP1(ACC(),6));
  (code.(2128) <- GROUP1(PUSH(),0));
  (code.(2129) <- GROUP1(ACC(),2));
  (code.(2130) <- GROUP1(PUSH(),0));
  (code.(2131) <- GROUP1(ACC(),4));
  (code.(2132) <- GROUP1(PUSH(),0));
  (code.(2133) <- GROUP1(ACC(),6));
  (code.(2134) <- GROUP1(PUSH(),0));
  (code.(2135) <- GROUP1(ACC(),8));
  (code.(2136) <- CLOSUREREC(2,6,2582,1666));
  (code.(2137) <- CLOSUREREC(1,0,2584,1806));
  (code.(2138) <- GROUP1(ACC(),7));
  (code.(2139) <- GROUP1(PUSH(),0));
  (code.(2140) <- GROUP1(ACC(),9));
  (code.(2141) <- GROUP1(PUSH(),0));
  (code.(2142) <- GROUP1(ACC(),2));
  (code.(2143) <- GROUP1(PUSH(),0));
  (code.(2144) <- GROUP1(ACC(),4));
  (code.(2145) <- GROUP1(PUSH(),0));
  (code.(2146) <- GROUP1(ACC(),6));
  (code.(2147) <- GROUP1(PUSH(),0));
  (code.(2148) <- GROUP1(ACC(),10));
  (code.(2149) <- GROUP1(PUSH(),0));
  (code.(2150) <- GROUP1(ACC(),12));
  (code.(2151) <- MAKEBLOCK(false,true,7,closure_tag,1841));
  (code.(2152) <- GROUP1(PUSH(),0));
  (code.(2153) <- GROUP1(ACC(),0));
  (code.(2154) <- GROUP1(PUSH(),0));
  (code.(2155) <- GROUP1(ACC(),2));
  (code.(2156) <- GROUP1(PUSH(),0));
  (code.(2157) <- GROUP1(ACC(),4));
  (code.(2158) <- GROUP1(PUSH(),0));
  (code.(2159) <- GROUP1(ACC(),6));
  (code.(2160) <- GROUP1(PUSH(),0));
  (code.(2161) <- GROUP1(ACC(),8));
  (code.(2162) <- GROUP1(PUSH(),0));
  (code.(2163) <- GROUP1(ACC(),10));
  (code.(2164) <- GROUP1(PUSH(),0));
  (code.(2165) <- GROUP1(ACC(),12));
  (code.(2166) <- GROUP1(PUSH(),0));
  (code.(2167) <- GROUP1(ACC(),14));
  (code.(2168) <- MAKEBLOCK(false,false,8,0,0));
  (code.(2169) <- RETURN(10));
  (code.(2170) <- GROUP1(CONST(0),0));
  (code.(2171) <- RETURN(1));
  (code.(2172) <- GROUP1(GETGLOBAL(),80));
  (code.(2173) <- RETURN(1));
  (code.(2174) <- MAKEBLOCK(false,true,0,closure_tag,2172));
  (code.(2175) <- GROUP1(SETGLOBAL(),27));
  (code.(2176) <- MAKEBLOCK(false,true,0,closure_tag,2170));
  (code.(2177) <- GROUP1(SETGLOBAL(),29));
  (code.(2178) <- MAKEBLOCK(false,true,0,closure_tag,2102));
  (code.(2179) <- GROUP1(SETGLOBAL(),69));
  (code.(2180) <- MAKEBLOCK(false,true,0,closure_tag,1653));
  (code.(2181) <- GROUP1(PUSH(),0));
  (code.(2182) <- MAKEBLOCK(false,true,0,closure_tag,1640));
  (code.(2183) <- GROUP1(PUSH(),0));
  (code.(2184) <- MAKEBLOCK(false,true,0,closure_tag,1427));
  (code.(2185) <- GROUP1(PUSH(),0));
  (code.(2186) <- GROUP1(CONST(7),0));
  (code.(2187) <- GROUP1(PUSH(),0));
  (code.(2188) <- GROUP1(SETGLOBAL(),64));
  (code.(2189) <- GROUP1(ACC(),0));
  (code.(2190) <- GROUP1(POP(),1));
  (code.(2191) <- GROUP1(PUSH(),0));
  (code.(2192) <- GROUP1(CONST(6),0));
  (code.(2193) <- GROUP1(PUSH(),0));
  (code.(2194) <- GROUP1(SETGLOBAL(),63));
  (code.(2195) <- GROUP1(ACC(),0));
  (code.(2196) <- GROUP1(POP(),1));
  (code.(2197) <- GROUP1(PUSH(),0));
  (code.(2198) <- MAKEBLOCK(false,true,0,closure_tag,1222));
  (code.(2199) <- GROUP1(PUSH(),0));
  (code.(2200) <- MAKEBLOCK(false,true,0,closure_tag,1180));
  (code.(2201) <- GROUP1(PUSH(),0));
  (code.(2202) <- MAKEBLOCK(false,true,0,closure_tag,1156));
  (code.(2203) <- GROUP1(PUSH(),0));
  (code.(2204) <- GROUP1(SETGLOBAL(),62));
  (code.(2205) <- GROUP1(ACC(),0));
  (code.(2206) <- GROUP1(POP(),1));
  (code.(2207) <- GROUP1(PUSH(),0));
  (code.(2208) <- MAKEBLOCK(false,true,0,closure_tag,1129));
  (code.(2209) <- GROUP1(PUSH(),0));
  (code.(2210) <- GROUP1(SETGLOBAL(),59));
  (code.(2211) <- GROUP1(ACC(),0));
  (code.(2212) <- GROUP1(POP(),1));
  (code.(2213) <- GROUP1(PUSH(),0));
  (code.(2214) <- MAKEBLOCK(false,true,0,closure_tag,1109));
  (code.(2215) <- GROUP1(PUSH(),0));
  (code.(2216) <- GROUP1(ACC(),0));
  (code.(2217) <- GROUP1(PUSH(),0));
  (code.(2218) <- GROUP1(ACC(),2));
  (code.(2219) <- GROUP1(PUSH(),0));
  (code.(2220) <- GROUP1(ACC(),4));
  (code.(2221) <- GROUP1(PUSH(),0));
  (code.(2222) <- GROUP1(ACC(),6));
  (code.(2223) <- GROUP1(PUSH(),0));
  (code.(2224) <- GROUP1(ACC(),8));
  (code.(2225) <- GROUP1(PUSH(),0));
  (code.(2226) <- GROUP1(ACC(),10));
  (code.(2227) <- GROUP1(PUSH(),0));
  (code.(2228) <- GROUP1(ACC(),12));
  (code.(2229) <- MAKEBLOCK(false,false,7,0,0));
  (code.(2230) <- GROUP1(PUSH(),0));
  (code.(2231) <- GROUP1(SETGLOBAL(),26));
  (code.(2232) <- GROUP1(ACC(),0));
  (code.(2233) <- GROUP1(POP(),1));
  (code.(2234) <- GROUP1(POP(),7));
  (code.(2235) <- GROUP1(PUSH(),0));
  (code.(2236) <- GROUP1(GETGLOBAL(),81));
  (code.(2237) <- GROUP1(PUSH(),0));
  (code.(2238) <- GROUP1(GETGLOBAL(),82));
  (code.(2239) <- APPLY(true,1,0));
  (code.(2240) <- GROUP1(PUSH(),0));
  (code.(2241) <- GROUP1(SETGLOBAL(),58));
  (code.(2242) <- GROUP1(ACC(),0));
  (code.(2243) <- GROUP1(POP(),1));
  (code.(2244) <- GROUP1(PUSH(),0));
  (code.(2245) <- GROUP1(CONST(0),0));
  (code.(2246) <- (CALL(10,false,false,false,false)));
  (code.(2247) <- GROUP1(PUSH(),0));
  (code.(2248) <- GROUP1(GETGLOBAL(),83));
  (code.(2249) <- MAKEBLOCK(false,false,2,248,0));
  (code.(2250) <- GROUP1(PUSH(),0));
  (code.(2251) <- GROUP1(SETGLOBAL(),54));
  (code.(2252) <- GROUP1(ACC(),0));
  (code.(2253) <- GROUP1(POP(),1));
  (code.(2254) <- GROUP1(PUSH(),0));
  (code.(2255) <- GROUP1(CONST(0),0));
  (code.(2256) <- (CALL(10,false,false,false,false)));
  (code.(2257) <- GROUP1(PUSH(),0));
  (code.(2258) <- GROUP1(GETGLOBAL(),84));
  (code.(2259) <- MAKEBLOCK(false,false,2,248,0));
  (code.(2260) <- GROUP1(PUSH(),0));
  (code.(2261) <- GROUP1(SETGLOBAL(),57));
  (code.(2262) <- GROUP1(ACC(),0));
  (code.(2263) <- GROUP1(POP(),1));
  (code.(2264) <- GROUP1(PUSH(),0));
  (code.(2265) <- GROUP1(CONST(0),0));
  (code.(2266) <- (CALL(10,false,false,false,false)));
  (code.(2267) <- GROUP1(PUSH(),0));
  (code.(2268) <- GROUP1(GETGLOBAL(),85));
  (code.(2269) <- MAKEBLOCK(false,false,2,248,0));
  (code.(2270) <- GROUP1(PUSH(),0));
  (code.(2271) <- GROUP1(SETGLOBAL(),56));
  (code.(2272) <- GROUP1(ACC(),0));
  (code.(2273) <- GROUP1(POP(),1));
  (code.(2274) <- GROUP1(PUSH(),0));
  (code.(2275) <- GROUP1(CONST(-10000),0));
  (code.(2276) <- GROUP1(PUSH(),0));
  (code.(2277) <- GROUP1(SETGLOBAL(),52));
  (code.(2278) <- GROUP1(ACC(),0));
  (code.(2279) <- GROUP1(POP(),1));
  (code.(2280) <- GROUP1(PUSH(),0));
  (code.(2281) <- GROUP1(CONST(10000),0));
  (code.(2282) <- GROUP1(PUSH(),0));
  (code.(2283) <- GROUP1(SETGLOBAL(),51));
  (code.(2284) <- GROUP1(ACC(),0));
  (code.(2285) <- GROUP1(POP(),1));
  (code.(2286) <- GROUP1(PUSH(),0));
  (code.(2287) <- MAKEBLOCK(false,true,0,closure_tag,960));
  (code.(2288) <- GROUP1(PUSH(),0));
  (code.(2289) <- GROUP1(SETGLOBAL(),55));
  (code.(2290) <- GROUP1(ACC(),0));
  (code.(2291) <- GROUP1(POP(),1));
  (code.(2292) <- GROUP1(PUSH(),0));
  (code.(2293) <- MAKEBLOCK(false,true,0,closure_tag,902));
  (code.(2294) <- GROUP1(PUSH(),0));
  (code.(2295) <- GROUP1(SETGLOBAL(),53));
  (code.(2296) <- GROUP1(ACC(),0));
  (code.(2297) <- GROUP1(POP(),1));
  (code.(2298) <- GROUP1(PUSH(),0));
  (code.(2299) <- MAKEBLOCK(false,true,0,closure_tag,741));
  (code.(2300) <- GROUP1(PUSH(),0));
  (code.(2301) <- GROUP1(SETGLOBAL(),50));
  (code.(2302) <- GROUP1(ACC(),0));
  (code.(2303) <- GROUP1(POP(),1));
  (code.(2304) <- GROUP1(PUSH(),0));
  (code.(2305) <- MAKEBLOCK(false,true,0,closure_tag,711));
  (code.(2306) <- GROUP1(PUSH(),0));
  (code.(2307) <- MAKEBLOCK(false,true,0,closure_tag,707));
  (code.(2308) <- GROUP1(PUSH(),0));
  (code.(2309) <- MAKEBLOCK(false,true,0,closure_tag,666));
  (code.(2310) <- GROUP1(PUSH(),0));
  (code.(2311) <- GROUP1(ACC(),0));
  (code.(2312) <- GROUP1(PUSH(),0));
  (code.(2313) <- GROUP1(ACC(),2));
  (code.(2314) <- GROUP1(PUSH(),0));
  (code.(2315) <- GROUP1(ACC(),4));
  (code.(2316) <- GROUP1(PUSH(),0));
  (code.(2317) <- GROUP1(ACC(),6));
  (code.(2318) <- GROUP1(PUSH(),0));
  (code.(2319) <- GROUP1(ACC(),8));
  (code.(2320) <- GROUP1(PUSH(),0));
  (code.(2321) <- GROUP1(ACC(),10));
  (code.(2322) <- GROUP1(PUSH(),0));
  (code.(2323) <- GROUP1(ACC(),12));
  (code.(2324) <- GROUP1(PUSH(),0));
  (code.(2325) <- GROUP1(ACC(),14));
  (code.(2326) <- GROUP1(PUSH(),0));
  (code.(2327) <- GROUP1(ACC(),16));
  (code.(2328) <- GROUP1(PUSH(),0));
  (code.(2329) <- GROUP1(ACC(),18));
  (code.(2330) <- GROUP1(PUSH(),0));
  (code.(2331) <- GROUP1(ACC(),20));
  (code.(2332) <- GROUP1(PUSH(),0));
  (code.(2333) <- GROUP1(ACC(),22));
  (code.(2334) <- MAKEBLOCK(false,false,12,0,0));
  (code.(2335) <- GROUP1(POP(),12));
  (code.(2336) <- GROUP1(PUSH(),0));
  (code.(2337) <- MAKEBLOCK(false,true,0,closure_tag,601));
  (code.(2338) <- GROUP1(PUSH(),0));
  (code.(2339) <- GROUP1(SETGLOBAL(),34));
  (code.(2340) <- GROUP1(ACC(),0));
  (code.(2341) <- GROUP1(POP(),1));
  (code.(2342) <- GROUP1(PUSH(),0));
  (code.(2343) <- MAKEBLOCK(false,true,0,closure_tag,598));
  (code.(2344) <- GROUP1(PUSH(),0));
  (code.(2345) <- MAKEBLOCK(false,true,0,closure_tag,595));
  (code.(2346) <- GROUP1(PUSH(),0));
  (code.(2347) <- MAKEBLOCK(false,true,0,closure_tag,583));
  (code.(2348) <- GROUP1(PUSH(),0));
  (code.(2349) <- GROUP1(SETGLOBAL(),40));
  (code.(2350) <- GROUP1(ACC(),0));
  (code.(2351) <- GROUP1(POP(),1));
  (code.(2352) <- GROUP1(PUSH(),0));
  (code.(2353) <- MAKEBLOCK(false,true,0,closure_tag,579));
  (code.(2354) <- GROUP1(PUSH(),0));
  (code.(2355) <- MAKEBLOCK(false,true,0,closure_tag,575));
  (code.(2356) <- GROUP1(PUSH(),0));
  (code.(2357) <- MAKEBLOCK(false,true,0,closure_tag,571));
  (code.(2358) <- GROUP1(PUSH(),0));
  (code.(2359) <- MAKEBLOCK(false,true,0,closure_tag,566));
  (code.(2360) <- GROUP1(PUSH(),0));
  (code.(2361) <- MAKEBLOCK(false,true,0,closure_tag,561));
  (code.(2362) <- GROUP1(PUSH(),0));
  (code.(2363) <- MAKEBLOCK(false,true,0,closure_tag,556));
  (code.(2364) <- GROUP1(PUSH(),0));
  (code.(2365) <- MAKEBLOCK(false,true,0,closure_tag,539));
  (code.(2366) <- GROUP1(PUSH(),0));
  (code.(2367) <- MAKEBLOCK(false,true,0,closure_tag,534));
  (code.(2368) <- GROUP1(PUSH(),0));
  (code.(2369) <- MAKEBLOCK(false,true,0,closure_tag,526));
  (code.(2370) <- GROUP1(PUSH(),0));
  (code.(2371) <- GROUP1(SETGLOBAL(),28));
  (code.(2372) <- GROUP1(ACC(),0));
  (code.(2373) <- GROUP1(POP(),1));
  (code.(2374) <- GROUP1(PUSH(),0));
  (code.(2375) <- GROUP1(CONST(0),0));
  (code.(2376) <- (CALL(10,false,false,false,false)));
  (code.(2377) <- GROUP1(PUSH(),0));
  (code.(2378) <- GROUP1(GETGLOBAL(),86));
  (code.(2379) <- MAKEBLOCK(false,false,2,248,0));
  (code.(2380) <- GROUP1(PUSH(),0));
  (code.(2381) <- GROUP1(SETGLOBAL(),31));
  (code.(2382) <- GROUP1(ACC(),0));
  (code.(2383) <- GROUP1(POP(),1));
  (code.(2384) <- GROUP1(PUSH(),0));
  (code.(2385) <- CLOSUREREC(1,0,2585,427));
  (code.(2386) <- GROUP1(ACC(),0));
  (code.(2387) <- GROUP1(PUSH(),0));
  (code.(2388) <- GROUP1(ACC(),2));
  (code.(2389) <- GROUP1(PUSH(),0));
  (code.(2390) <- GROUP1(ACC(),4));
  (code.(2391) <- GROUP1(PUSH(),0));
  (code.(2392) <- GROUP1(ACC(),6));
  (code.(2393) <- GROUP1(PUSH(),0));
  (code.(2394) <- GROUP1(ACC(),8));
  (code.(2395) <- GROUP1(PUSH(),0));
  (code.(2396) <- GROUP1(ACC(),10));
  (code.(2397) <- GROUP1(PUSH(),0));
  (code.(2398) <- GROUP1(ACC(),12));
  (code.(2399) <- GROUP1(PUSH(),0));
  (code.(2400) <- GROUP1(ACC(),14));
  (code.(2401) <- GROUP1(PUSH(),0));
  (code.(2402) <- GROUP1(ACC(),16));
  (code.(2403) <- GROUP1(PUSH(),0));
  (code.(2404) <- GROUP1(ACC(),18));
  (code.(2405) <- GROUP1(PUSH(),0));
  (code.(2406) <- GROUP1(ACC(),20));
  (code.(2407) <- GROUP1(PUSH(),0));
  (code.(2408) <- GROUP1(ACC(),22));
  (code.(2409) <- GROUP1(PUSH(),0));
  (code.(2410) <- GROUP1(ACC(),24));
  (code.(2411) <- GROUP1(PUSH(),0));
  (code.(2412) <- GROUP1(ACC(),26));
  (code.(2413) <- GROUP1(PUSH(),0));
  (code.(2414) <- GROUP1(ACC(),28));
  (code.(2415) <- MAKEBLOCK(false,false,15,0,0));
  (code.(2416) <- GROUP1(POP(),15));
  (code.(2417) <- GROUP1(PUSH(),0));
  (code.(2418) <- GROUP1(ACC(),1));
  (code.(2419) <- GROUP1(GETFIELD(),11));
  (code.(2420) <- GROUP1(PUSH(),0));
  (code.(2421) <- GROUP1(ACC(),2));
  (code.(2422) <- GROUP1(GETFIELD(),10));
  (code.(2423) <- GROUP1(PUSH(),0));
  (code.(2424) <- GROUP1(ACC(),3));
  (code.(2425) <- GROUP1(GETFIELD(),9));
  (code.(2426) <- GROUP1(PUSH(),0));
  (code.(2427) <- GROUP1(ACC(),4));
  (code.(2428) <- GROUP1(GETFIELD(),4));
  (code.(2429) <- GROUP1(PUSH(),0));
  (code.(2430) <- GROUP1(ACC(),5));
  (code.(2431) <- GROUP1(GETFIELD(),5));
  (code.(2432) <- GROUP1(PUSH(),0));
  (code.(2433) <- GROUP1(ACC(),6));
  (code.(2434) <- GROUP1(GETFIELD(),8));
  (code.(2435) <- MAKEBLOCK(false,false,6,0,0));
  (code.(2436) <- GROUP1(PUSH(),0));
  (code.(2437) <- GROUP1(ACC(),3));
  (code.(2438) <- GROUP1(GETFIELD(),6));
  (code.(2439) <- GROUP1(PUSH(),0));
  (code.(2440) <- GROUP1(ACC(),4));
  (code.(2441) <- GROUP1(GETFIELD(),3));
  (code.(2442) <- GROUP1(PUSH(),0));
  (code.(2443) <- GROUP1(ACC(),5));
  (code.(2444) <- GROUP1(GETFIELD(),2));
  (code.(2445) <- MAKEBLOCK(false,false,3,0,0));
  (code.(2446) <- GROUP1(PUSH(),0));
  (code.(2447) <- GROUP1(ACC(),7));
  (code.(2448) <- APPLY(true,1,0));
  (code.(2449) <- APPLY(true,1,0));
  (code.(2450) <- GROUP1(PUSH(),0));
  (code.(2451) <- GROUP1(ACC(),2));
  (code.(2452) <- GROUP1(GETFIELD(),11));
  (code.(2453) <- GROUP1(PUSH(),0));
  (code.(2454) <- GROUP1(ACC(),3));
  (code.(2455) <- GROUP1(GETFIELD(),10));
  (code.(2456) <- GROUP1(PUSH(),0));
  (code.(2457) <- GROUP1(ACC(),4));
  (code.(2458) <- GROUP1(GETFIELD(),9));
  (code.(2459) <- GROUP1(PUSH(),0));
  (code.(2460) <- GROUP1(ACC(),5));
  (code.(2461) <- GROUP1(GETFIELD(),4));
  (code.(2462) <- GROUP1(PUSH(),0));
  (code.(2463) <- GROUP1(ACC(),6));
  (code.(2464) <- GROUP1(GETFIELD(),5));
  (code.(2465) <- GROUP1(PUSH(),0));
  (code.(2466) <- GROUP1(ACC(),7));
  (code.(2467) <- GROUP1(GETFIELD(),8));
  (code.(2468) <- MAKEBLOCK(false,false,6,0,0));
  (code.(2469) <- GROUP1(PUSH(),0));
  (code.(2470) <- GROUP1(ACC(),2));
  (code.(2471) <- GROUP1(GETFIELD(),5));
  (code.(2472) <- GROUP1(PUSH(),0));
  (code.(2473) <- GROUP1(ACC(),3));
  (code.(2474) <- GROUP1(GETFIELD(),4));
  (code.(2475) <- GROUP1(PUSH(),0));
  (code.(2476) <- GROUP1(ACC(),4));
  (code.(2477) <- GROUP1(GETFIELD(),6));
  (code.(2478) <- GROUP1(PUSH(),0));
  (code.(2479) <- GROUP1(ACC(),5));
  (code.(2480) <- GROUP1(GETFIELD(),14));
  (code.(2481) <- GROUP1(PUSH(),0));
  (code.(2482) <- GROUP1(ACC(),6));
  (code.(2483) <- GROUP1(GETFIELD(),11));
  (code.(2484) <- GROUP1(PUSH(),0));
  (code.(2485) <- GROUP1(ACC(),7));
  (code.(2486) <- GROUP1(GETFIELD(),10));
  (code.(2487) <- GROUP1(PUSH(),0));
  (code.(2488) <- GROUP1(ACC(),8));
  (code.(2489) <- GROUP1(GETFIELD(),9));
  (code.(2490) <- GROUP1(PUSH(),0));
  (code.(2491) <- GROUP1(ACC(),9));
  (code.(2492) <- GROUP1(GETFIELD(),8));
  (code.(2493) <- GROUP1(PUSH(),0));
  (code.(2494) <- GROUP1(ACC(),10));
  (code.(2495) <- GROUP1(GETFIELD(),7));
  (code.(2496) <- GROUP1(PUSH(),0));
  (code.(2497) <- GROUP1(ACC(),11));
  (code.(2498) <- GROUP1(GETFIELD(),2));
  (code.(2499) <- GROUP1(PUSH(),0));
  (code.(2500) <- GROUP1(ACC(),12));
  (code.(2501) <- GROUP1(GETFIELD(),1));
  (code.(2502) <- MAKEBLOCK(false,false,11,0,0));
  (code.(2503) <- GROUP1(PUSH(),0));
  (code.(2504) <- GROUP1(ACC(),5));
  (code.(2505) <- GROUP1(GETFIELD(),6));
  (code.(2506) <- GROUP1(PUSH(),0));
  (code.(2507) <- GROUP1(ACC(),6));
  (code.(2508) <- GROUP1(GETFIELD(),3));
  (code.(2509) <- GROUP1(PUSH(),0));
  (code.(2510) <- GROUP1(ACC(),7));
  (code.(2511) <- GROUP1(GETFIELD(),2));
  (code.(2512) <- MAKEBLOCK(false,false,3,0,0));
  (code.(2513) <- GROUP1(PUSH(),0));
  (code.(2514) <- GROUP1(ACC(),7));
  (code.(2515) <- APPLY(true,1,0));
  (code.(2516) <- APPLY(true,1,0));
  (code.(2517) <- APPLY(true,1,0));
  (code.(2518) <- APPLY(true,1,0));
  (code.(2519) <- GROUP1(PUSH(),0));
  (code.(2520) <- GROUP1(ACC(),0));
  (code.(2521) <- GROUP1(GETFIELD(),6));
  (code.(2522) <- GROUP1(PUSH(),0));
  (code.(2523) <- GROUP1(ACC(),1));
  (code.(2524) <- GROUP1(GETFIELD(),5));
  (code.(2525) <- GROUP1(PUSH(),0));
  (code.(2526) <- GROUP1(ACC(),2));
  (code.(2527) <- GROUP1(GETFIELD(),4));
  (code.(2528) <- GROUP1(PUSH(),0));
  (code.(2529) <- GROUP1(ACC(),3));
  (code.(2530) <- GROUP1(GETFIELD(),3));
  (code.(2531) <- GROUP1(PUSH(),0));
  (code.(2532) <- GROUP1(ACC(),4));
  (code.(2533) <- GROUP1(GETFIELD(),2));
  (code.(2534) <- GROUP1(PUSH(),0));
  (code.(2535) <- GROUP1(ACC(),5));
  (code.(2536) <- GROUP1(GETFIELD(),1));
  (code.(2537) <- GROUP1(PUSH(),0));
  (code.(2538) <- GROUP1(ACC(),6));
  (code.(2539) <- GROUP1(GETFIELD(),9));
  (code.(2540) <- GROUP1(PUSH(),0));
  (code.(2541) <- GROUP1(ACC(),7));
  (code.(2542) <- GROUP1(GETFIELD(),7));
  (code.(2543) <- GROUP1(PUSH(),0));
  (code.(2544) <- GROUP1(ACC(),8));
  (code.(2545) <- GROUP1(GETFIELD(),13));
  (code.(2546) <- GROUP1(PUSH(),0));
  (code.(2547) <- GROUP1(ACC(),9));
  (code.(2548) <- GROUP1(GETFIELD(),10));
  (code.(2549) <- MAKEBLOCK(false,false,10,0,0));
  (code.(2550) <- GROUP1(PUSH(),0));
  (code.(2551) <- GROUP1(ACC(),6));
  (code.(2552) <- APPLY(true,1,0));
  (code.(2553) <- GROUP1(PUSH(),0));
  (code.(2554) <- GROUP1(CONST(0),0));
  (code.(2555) <- GROUP1(PUSH(),0));
  (code.(2556) <- GROUP1(ACC(),1));
  (code.(2557) <- GROUP1(GETFIELD(),1));
  (code.(2558) <- APPLY(true,1,0));
  (code.(2559) <- GROUP1(POP(),8));
  (code.(2560) <- GROUP3(STOP()));
  (code.(2561) <- LABEL(161));
  (code.(2562) <- LABEL(82));
  (code.(2563) <- LABEL(96));
  (code.(2564) <- LABEL(113));
  (code.(2565) <- LABEL(134));
  (code.(2566) <- LABEL(248));
  (code.(2567) <- LABEL(234));
  (code.(2568) <- LABEL(633));
  (code.(2569) <- LABEL(636));
  (code.(2570) <- LABEL(639));
  (code.(2571) <- LABEL(1013));
  (code.(2572) <- LABEL(1036));
  (code.(2573) <- LABEL(1059));
  (code.(2574) <- LABEL(1359));
  (code.(2575) <- LABEL(1361));
  (code.(2576) <- LABEL(1363));
  (code.(2577) <- LABEL(1365));
  (code.(2578) <- LABEL(1418));
  (code.(2579) <- LABEL(1420));
  (code.(2580) <- LABEL(1422));
  (code.(2581) <- LABEL(1424));
  (code.(2582) <- LABEL(1666));
  (code.(2583) <- LABEL(1736));
  (code.(2584) <- LABEL(1806));
  (code.(2585) <- LABEL(427));
  ()
 ;;

let init_data () = (
let x1 = global_start + 87 in
(* ADD GLOBAL 12 *)
data_rom.(global_start + 12) <- val_long(0);
(* ADD GLOBAL 13 *)
(* ========= *)
data_rom.(x1) <- val_long(make_header(252,3));
let x2 = x1+4 in
data_rom.(x1+1) <- val_long(110);
data_rom.(x1+2) <- val_long(116);
data_rom.(x1+3) <- val_long(104);
data_rom.(global_start + 13) <- val_ptr(x1);
(* ADD GLOBAL 14 *)
data_rom.(global_start + 14) <- val_long(0);
(* ADD GLOBAL 15 *)
(* ========= *)
data_rom.(x2) <- val_long(make_header(252,8));
let x3 = x2+9 in
data_rom.(x2+1) <- val_long(76);
data_rom.(x2+2) <- val_long(105);
data_rom.(x2+3) <- val_long(115);
data_rom.(x2+4) <- val_long(116);
data_rom.(x2+5) <- val_long(46);
data_rom.(x2+6) <- val_long(110);
data_rom.(x2+7) <- val_long(116);
data_rom.(x2+8) <- val_long(104);
data_rom.(global_start + 15) <- val_ptr(x2);
(* ADD GLOBAL 16 *)
data_rom.(global_start + 16) <- val_long(0);
(* ADD GLOBAL 17 *)
(* ========= *)
data_rom.(x3) <- val_long(make_header(252,2));
let x4 = x3+3 in
data_rom.(x3+1) <- val_long(104);
data_rom.(x3+2) <- val_long(100);
data_rom.(global_start + 17) <- val_ptr(x3);
(* ADD GLOBAL 18 *)
data_rom.(global_start + 18) <- val_long(0);
(* ADD GLOBAL 19 *)
data_rom.(global_start + 19) <- val_long(0);
(* ADD GLOBAL 20 *)
(* ========= *)
data_rom.(x4) <- val_long(make_header(252,17));
let x5 = x4+18 in
data_rom.(x4+1) <- val_long(65);
data_rom.(x4+2) <- val_long(114);
data_rom.(x4+3) <- val_long(114);
data_rom.(x4+4) <- val_long(97);
data_rom.(x4+5) <- val_long(121);
data_rom.(x4+6) <- val_long(46);
data_rom.(x4+7) <- val_long(109);
data_rom.(x4+8) <- val_long(97);
data_rom.(x4+9) <- val_long(107);
data_rom.(x4+10) <- val_long(101);
data_rom.(x4+11) <- val_long(95);
data_rom.(x4+12) <- val_long(109);
data_rom.(x4+13) <- val_long(97);
data_rom.(x4+14) <- val_long(116);
data_rom.(x4+15) <- val_long(114);
data_rom.(x4+16) <- val_long(105);
data_rom.(x4+17) <- val_long(120);
data_rom.(global_start + 20) <- val_ptr(x4);
(* ADD GLOBAL 21 *)
(* ========= *)
data_rom.(x5) <- val_long(make_header(252,3));
let x6 = x5+4 in
data_rom.(x5+1) <- val_long(32);
data_rom.(x5+2) <- val_long(58);
data_rom.(x5+3) <- val_long(32);
data_rom.(global_start + 21) <- val_ptr(x5);
(* ADD GLOBAL 22 *)
(* ========= *)
data_rom.(x6) <- val_long(make_header(252,1));
let x7 = x6+2 in
data_rom.(x6+1) <- val_long(49);
data_rom.(global_start + 22) <- val_ptr(x6);
(* ADD GLOBAL 23 *)
(* ========= *)
data_rom.(x7) <- val_long(make_header(252,1));
let x8 = x7+2 in
data_rom.(x7+1) <- val_long(50);
data_rom.(global_start + 23) <- val_ptr(x7);
(* ADD GLOBAL 24 *)
data_rom.(global_start + 24) <- val_long(0);
(* ADD GLOBAL 25 *)
(* ========= *)
data_rom.(x8) <- val_long(make_header(252,12));
let x9 = x8+13 in
data_rom.(x8+1) <- val_long(67);
data_rom.(x8+2) <- val_long(104);
data_rom.(x8+3) <- val_long(111);
data_rom.(x8+4) <- val_long(105);
data_rom.(x8+5) <- val_long(120);
data_rom.(x8+6) <- val_long(32);
data_rom.(x8+7) <- val_long(106);
data_rom.(x8+8) <- val_long(111);
data_rom.(x8+9) <- val_long(117);
data_rom.(x8+10) <- val_long(101);
data_rom.(x8+11) <- val_long(117);
data_rom.(x8+12) <- val_long(114);
data_rom.(global_start + 25) <- val_ptr(x8);
(* ADD GLOBAL 26 *)
data_rom.(global_start + 26) <- val_long(0);
(* ADD GLOBAL 27 *)
data_rom.(global_start + 27) <- val_long(0);
(* ADD GLOBAL 28 *)
data_rom.(global_start + 28) <- val_long(0);
(* ADD GLOBAL 29 *)
data_rom.(global_start + 29) <- val_long(0);
(* ADD GLOBAL 30 *)
data_rom.(global_start + 30) <- val_long(0);
(* ADD GLOBAL 31 *)
data_rom.(global_start + 31) <- val_long(0);
(* ADD GLOBAL 32 *)
(* ========= *)
data_rom.(x9) <- val_long(make_header(252,35));
let x10 = x9+36 in
data_rom.(x9+1) <- val_long(99);
data_rom.(x9+2) <- val_long(111);
data_rom.(x9+3) <- val_long(117);
data_rom.(x9+4) <- val_long(112);
data_rom.(x9+5) <- val_long(32);
data_rom.(x9+6) <- val_long(110);
data_rom.(x9+7) <- val_long(111);
data_rom.(x9+8) <- val_long(110);
data_rom.(x9+9) <- val_long(32);
data_rom.(x9+10) <- val_long(118);
data_rom.(x9+11) <- val_long(97);
data_rom.(x9+12) <- val_long(108);
data_rom.(x9+13) <- val_long(97);
data_rom.(x9+14) <- val_long(98);
data_rom.(x9+15) <- val_long(108);
data_rom.(x9+16) <- val_long(101);
data_rom.(x9+17) <- val_long(44);
data_rom.(x9+18) <- val_long(32);
data_rom.(x9+19) <- val_long(101);
data_rom.(x9+20) <- val_long(115);
data_rom.(x9+21) <- val_long(115);
data_rom.(x9+22) <- val_long(97);
data_rom.(x9+23) <- val_long(121);
data_rom.(x9+24) <- val_long(101);
data_rom.(x9+25) <- val_long(122);
data_rom.(x9+26) <- val_long(32);
data_rom.(x9+27) <- val_long(101);
data_rom.(x9+28) <- val_long(110);
data_rom.(x9+29) <- val_long(99);
data_rom.(x9+30) <- val_long(111);
data_rom.(x9+31) <- val_long(114);
data_rom.(x9+32) <- val_long(101);
data_rom.(x9+33) <- val_long(32);
data_rom.(x9+34) <- val_long(58);
data_rom.(x9+35) <- val_long(32);
data_rom.(global_start + 32) <- val_ptr(x9);
(* ADD GLOBAL 33 *)
data_rom.(global_start + 33) <- val_long(0);
(* ADD GLOBAL 34 *)
data_rom.(global_start + 34) <- val_long(0);
(* ADD GLOBAL 35 *)
(* ========= *)
data_rom.(x10) <- val_long(make_header(252,30));
let x11 = x10+31 in
data_rom.(x10+1) <- val_long(88);
data_rom.(x10+2) <- val_long(58);
data_rom.(x10+3) <- val_long(32);
data_rom.(x10+4) <- val_long(49);
data_rom.(x10+5) <- val_long(101);
data_rom.(x10+6) <- val_long(114);
data_rom.(x10+7) <- val_long(32);
data_rom.(x10+8) <- val_long(106);
data_rom.(x10+9) <- val_long(111);
data_rom.(x10+10) <- val_long(117);
data_rom.(x10+11) <- val_long(101);
data_rom.(x10+12) <- val_long(117);
data_rom.(x10+13) <- val_long(114);
data_rom.(x10+14) <- val_long(32);
data_rom.(x10+15) <- val_long(32);
data_rom.(x10+16) <- val_long(32);
data_rom.(x10+17) <- val_long(79);
data_rom.(x10+18) <- val_long(58);
data_rom.(x10+19) <- val_long(32);
data_rom.(x10+20) <- val_long(50);
data_rom.(x10+21) <- val_long(101);
data_rom.(x10+22) <- val_long(109);
data_rom.(x10+23) <- val_long(101);
data_rom.(x10+24) <- val_long(32);
data_rom.(x10+25) <- val_long(106);
data_rom.(x10+26) <- val_long(111);
data_rom.(x10+27) <- val_long(117);
data_rom.(x10+28) <- val_long(101);
data_rom.(x10+29) <- val_long(117);
data_rom.(x10+30) <- val_long(114);
data_rom.(global_start + 35) <- val_ptr(x10);
(* ADD GLOBAL 36 *)
(* ========= *)
data_rom.(x11) <- val_long(make_header(252,12));
let x12 = x11+13 in
data_rom.(x11+1) <- val_long(80);
data_rom.(x11+2) <- val_long(97);
data_rom.(x11+3) <- val_long(114);
data_rom.(x11+4) <- val_long(116);
data_rom.(x11+5) <- val_long(105);
data_rom.(x11+6) <- val_long(101);
data_rom.(x11+7) <- val_long(32);
data_rom.(x11+8) <- val_long(110);
data_rom.(x11+9) <- val_long(117);
data_rom.(x11+10) <- val_long(108);
data_rom.(x11+11) <- val_long(108);
data_rom.(x11+12) <- val_long(101);
data_rom.(global_start + 36) <- val_ptr(x11);
(* ADD GLOBAL 37 *)
(* ========= *)
data_rom.(x12) <- val_long(make_header(252,18));
let x13 = x12+19 in
data_rom.(x12+1) <- val_long(76);
data_rom.(x12+2) <- val_long(101);
data_rom.(x12+3) <- val_long(32);
data_rom.(x12+4) <- val_long(49);
data_rom.(x12+5) <- val_long(101);
data_rom.(x12+6) <- val_long(114);
data_rom.(x12+7) <- val_long(32);
data_rom.(x12+8) <- val_long(106);
data_rom.(x12+9) <- val_long(111);
data_rom.(x12+10) <- val_long(117);
data_rom.(x12+11) <- val_long(101);
data_rom.(x12+12) <- val_long(117);
data_rom.(x12+13) <- val_long(114);
data_rom.(x12+14) <- val_long(32);
data_rom.(x12+15) <- val_long(112);
data_rom.(x12+16) <- val_long(101);
data_rom.(x12+17) <- val_long(114);
data_rom.(x12+18) <- val_long(100);
data_rom.(global_start + 37) <- val_ptr(x12);
(* ADD GLOBAL 38 *)
(* ========= *)
data_rom.(x13) <- val_long(make_header(252,19));
let x14 = x13+20 in
data_rom.(x13+1) <- val_long(76);
data_rom.(x13+2) <- val_long(101);
data_rom.(x13+3) <- val_long(32);
data_rom.(x13+4) <- val_long(49);
data_rom.(x13+5) <- val_long(101);
data_rom.(x13+6) <- val_long(114);
data_rom.(x13+7) <- val_long(32);
data_rom.(x13+8) <- val_long(106);
data_rom.(x13+9) <- val_long(111);
data_rom.(x13+10) <- val_long(117);
data_rom.(x13+11) <- val_long(101);
data_rom.(x13+12) <- val_long(117);
data_rom.(x13+13) <- val_long(114);
data_rom.(x13+14) <- val_long(32);
data_rom.(x13+15) <- val_long(103);
data_rom.(x13+16) <- val_long(97);
data_rom.(x13+17) <- val_long(103);
data_rom.(x13+18) <- val_long(110);
data_rom.(x13+19) <- val_long(101);
data_rom.(global_start + 38) <- val_ptr(x13);
(* ADD GLOBAL 39 *)
(* ========= *)
data_rom.(x14) <- val_long(make_header(252,29));
let x15 = x14+30 in
data_rom.(x14+1) <- val_long(69);
data_rom.(x14+2) <- val_long(115);
data_rom.(x14+3) <- val_long(116);
data_rom.(x14+4) <- val_long(45);
data_rom.(x14+5) <- val_long(99);
data_rom.(x14+6) <- val_long(101);
data_rom.(x14+7) <- val_long(32);
data_rom.(x14+8) <- val_long(117);
data_rom.(x14+9) <- val_long(110);
data_rom.(x14+10) <- val_long(101);
data_rom.(x14+11) <- val_long(32);
data_rom.(x14+12) <- val_long(109);
data_rom.(x14+13) <- val_long(97);
data_rom.(x14+14) <- val_long(99);
data_rom.(x14+15) <- val_long(104);
data_rom.(x14+16) <- val_long(105);
data_rom.(x14+17) <- val_long(110);
data_rom.(x14+18) <- val_long(101);
data_rom.(x14+19) <- val_long(32);
data_rom.(x14+20) <- val_long(113);
data_rom.(x14+21) <- val_long(117);
data_rom.(x14+22) <- val_long(105);
data_rom.(x14+23) <- val_long(32);
data_rom.(x14+24) <- val_long(106);
data_rom.(x14+25) <- val_long(111);
data_rom.(x14+26) <- val_long(117);
data_rom.(x14+27) <- val_long(101);
data_rom.(x14+28) <- val_long(32);
data_rom.(x14+29) <- val_long(63);
data_rom.(global_start + 39) <- val_ptr(x14);
(* ADD GLOBAL 40 *)
data_rom.(global_start + 40) <- val_long(0);
(* ADD GLOBAL 41 *)
(* ========= *)
data_rom.(x15) <- val_long(make_header(252,17));
let x16 = x15+18 in
data_rom.(x15+1) <- val_long(69);
data_rom.(x15+2) <- val_long(110);
data_rom.(x15+3) <- val_long(99);
data_rom.(x15+4) <- val_long(111);
data_rom.(x15+5) <- val_long(114);
data_rom.(x15+6) <- val_long(101);
data_rom.(x15+7) <- val_long(32);
data_rom.(x15+8) <- val_long(117);
data_rom.(x15+9) <- val_long(110);
data_rom.(x15+10) <- val_long(101);
data_rom.(x15+11) <- val_long(32);
data_rom.(x15+12) <- val_long(112);
data_rom.(x15+13) <- val_long(97);
data_rom.(x15+14) <- val_long(114);
data_rom.(x15+15) <- val_long(116);
data_rom.(x15+16) <- val_long(105);
data_rom.(x15+17) <- val_long(101);
data_rom.(global_start + 41) <- val_ptr(x15);
(* ADD GLOBAL 42 *)
(* ========= *)
data_rom.(x16) <- val_long(make_header(252,21));
let x17 = x16+22 in
data_rom.(x16+1) <- val_long(86);
data_rom.(x16+2) <- val_long(111);
data_rom.(x16+3) <- val_long(117);
data_rom.(x16+4) <- val_long(108);
data_rom.(x16+5) <- val_long(101);
data_rom.(x16+6) <- val_long(122);
data_rom.(x16+7) <- val_long(45);
data_rom.(x16+8) <- val_long(118);
data_rom.(x16+9) <- val_long(111);
data_rom.(x16+10) <- val_long(117);
data_rom.(x16+11) <- val_long(115);
data_rom.(x16+12) <- val_long(32);
data_rom.(x16+13) <- val_long(99);
data_rom.(x16+14) <- val_long(111);
data_rom.(x16+15) <- val_long(109);
data_rom.(x16+16) <- val_long(109);
data_rom.(x16+17) <- val_long(101);
data_rom.(x16+18) <- val_long(110);
data_rom.(x16+19) <- val_long(99);
data_rom.(x16+20) <- val_long(101);
data_rom.(x16+21) <- val_long(114);
data_rom.(global_start + 42) <- val_ptr(x16);
(* ADD GLOBAL 43 *)
(* ========= *)
data_rom.(x17) <- val_long(make_header(252,7));
let x18 = x17+8 in
data_rom.(x17+1) <- val_long(32);
data_rom.(x17+2) <- val_long(111);
data_rom.(x17+3) <- val_long(47);
data_rom.(x17+4) <- val_long(110);
data_rom.(x17+5) <- val_long(32);
data_rom.(x17+6) <- val_long(63);
data_rom.(x17+7) <- val_long(32);
data_rom.(global_start + 43) <- val_ptr(x17);
(* ADD GLOBAL 44 *)
(* ========= *)
data_rom.(x18) <- val_long(make_header(252,1));
let x19 = x18+2 in
data_rom.(x18+1) <- val_long(111);
data_rom.(global_start + 44) <- val_ptr(x18);
(* ADD GLOBAL 45 *)
(* ========= *)
data_rom.(x19) <- val_long(make_header(252,15));
let x20 = x19+16 in
data_rom.(x19+1) <- val_long(65);
data_rom.(x19+2) <- val_long(32);
data_rom.(x19+3) <- val_long(98);
data_rom.(x19+4) <- val_long(105);
data_rom.(x19+5) <- val_long(101);
data_rom.(x19+6) <- val_long(110);
data_rom.(x19+7) <- val_long(116);
data_rom.(x19+8) <- val_long(111);
data_rom.(x19+9) <- val_long(116);
data_rom.(x19+10) <- val_long(32);
data_rom.(x19+11) <- val_long(46);
data_rom.(x19+12) <- val_long(46);
data_rom.(x19+13) <- val_long(46);
data_rom.(x19+14) <- val_long(32);
data_rom.(x19+15) <- val_long(10);
data_rom.(global_start + 45) <- val_ptr(x19);
(* ADD GLOBAL 46 *)
(* ========= *)
data_rom.(x20) <- val_long(make_header(252,7));
let x21 = x20+8 in
data_rom.(x20+1) <- val_long(80);
data_rom.(x20+2) <- val_long(52);
data_rom.(x20+3) <- val_long(32);
data_rom.(x20+4) <- val_long(46);
data_rom.(x20+5) <- val_long(46);
data_rom.(x20+6) <- val_long(46);
data_rom.(x20+7) <- val_long(10);
data_rom.(global_start + 46) <- val_ptr(x20);
(* ADD GLOBAL 47 *)
(* ========= *)
data_rom.(x21) <- val_long(make_header(252,2));
let x22 = x21+3 in
data_rom.(x21+1) <- val_long(88);
data_rom.(x21+2) <- val_long(32);
data_rom.(global_start + 47) <- val_ptr(x21);
(* ADD GLOBAL 48 *)
(* ========= *)
data_rom.(x22) <- val_long(make_header(252,2));
let x23 = x22+3 in
data_rom.(x22+1) <- val_long(79);
data_rom.(x22+2) <- val_long(32);
data_rom.(global_start + 48) <- val_ptr(x22);
(* ADD GLOBAL 49 *)
(* ========= *)
data_rom.(x23) <- val_long(make_header(252,2));
let x24 = x23+3 in
data_rom.(x23+1) <- val_long(46);
data_rom.(x23+2) <- val_long(32);
data_rom.(global_start + 49) <- val_ptr(x23);
(* ADD GLOBAL 50 *)
data_rom.(global_start + 50) <- val_long(0);
(* ADD GLOBAL 51 *)
data_rom.(global_start + 51) <- val_long(0);
(* ADD GLOBAL 52 *)
data_rom.(global_start + 52) <- val_long(0);
(* ADD GLOBAL 53 *)
data_rom.(global_start + 53) <- val_long(0);
(* ADD GLOBAL 54 *)
data_rom.(global_start + 54) <- val_long(0);
(* ADD GLOBAL 55 *)
data_rom.(global_start + 55) <- val_long(0);
(* ADD GLOBAL 56 *)
data_rom.(global_start + 56) <- val_long(0);
(* ADD GLOBAL 57 *)
data_rom.(global_start + 57) <- val_long(0);
(* ADD GLOBAL 58 *)
data_rom.(global_start + 58) <- val_long(0);
(* ADD GLOBAL 59 *)
data_rom.(global_start + 59) <- val_long(0);
(* ADD GLOBAL 60 *)
data_rom.(global_start + 60) <- val_long(0);
(* ADD GLOBAL 61 *)
data_rom.(global_start + 61) <- val_long(0);
(* ADD GLOBAL 62 *)
data_rom.(global_start + 62) <- val_long(0);
(* ADD GLOBAL 63 *)
data_rom.(global_start + 63) <- val_long(0);
(* ADD GLOBAL 64 *)
data_rom.(global_start + 64) <- val_long(0);
(* ADD GLOBAL 65 *)
data_rom.(global_start + 65) <- val_long(0);
(* ADD GLOBAL 66 *)
(* ========= *)
data_rom.(x24) <- val_long(make_header(252,47));
let x25 = x24+48 in
data_rom.(x24+1) <- val_long(80);
data_rom.(x24+2) <- val_long(52);
data_rom.(x24+3) <- val_long(95);
data_rom.(x24+4) <- val_long(116);
data_rom.(x24+5) <- val_long(111);
data_rom.(x24+6) <- val_long(117);
data_rom.(x24+7) <- val_long(116);
data_rom.(x24+8) <- val_long(46);
data_rom.(x24+9) <- val_long(70);
data_rom.(x24+10) <- val_long(83);
data_rom.(x24+11) <- val_long(113);
data_rom.(x24+12) <- val_long(117);
data_rom.(x24+13) <- val_long(101);
data_rom.(x24+14) <- val_long(108);
data_rom.(x24+15) <- val_long(101);
data_rom.(x24+16) <- val_long(116);
data_rom.(x24+17) <- val_long(116);
data_rom.(x24+18) <- val_long(101);
data_rom.(x24+19) <- val_long(40);
data_rom.(x24+20) <- val_long(82);
data_rom.(x24+21) <- val_long(101);
data_rom.(x24+22) <- val_long(112);
data_rom.(x24+23) <- val_long(41);
data_rom.(x24+24) <- val_long(40);
data_rom.(x24+25) <- val_long(65);
data_rom.(x24+26) <- val_long(102);
data_rom.(x24+27) <- val_long(102);
data_rom.(x24+28) <- val_long(41);
data_rom.(x24+29) <- val_long(40);
data_rom.(x24+30) <- val_long(69);
data_rom.(x24+31) <- val_long(118);
data_rom.(x24+32) <- val_long(97);
data_rom.(x24+33) <- val_long(108);
data_rom.(x24+34) <- val_long(41);
data_rom.(x24+35) <- val_long(40);
data_rom.(x24+36) <- val_long(65);
data_rom.(x24+37) <- val_long(108);
data_rom.(x24+38) <- val_long(112);
data_rom.(x24+39) <- val_long(104);
data_rom.(x24+40) <- val_long(97);
data_rom.(x24+41) <- val_long(41);
data_rom.(x24+42) <- val_long(46);
data_rom.(x24+43) <- val_long(71);
data_rom.(x24+44) <- val_long(97);
data_rom.(x24+45) <- val_long(103);
data_rom.(x24+46) <- val_long(110);
data_rom.(x24+47) <- val_long(101);
data_rom.(global_start + 66) <- val_ptr(x24);
(* ADD GLOBAL 67 *)
(* ========= *)
data_rom.(x25) <- val_long(make_header(252,46));
let x26 = x25+47 in
data_rom.(x25+1) <- val_long(80);
data_rom.(x25+2) <- val_long(52);
data_rom.(x25+3) <- val_long(95);
data_rom.(x25+4) <- val_long(116);
data_rom.(x25+5) <- val_long(111);
data_rom.(x25+6) <- val_long(117);
data_rom.(x25+7) <- val_long(116);
data_rom.(x25+8) <- val_long(46);
data_rom.(x25+9) <- val_long(70);
data_rom.(x25+10) <- val_long(83);
data_rom.(x25+11) <- val_long(113);
data_rom.(x25+12) <- val_long(117);
data_rom.(x25+13) <- val_long(101);
data_rom.(x25+14) <- val_long(108);
data_rom.(x25+15) <- val_long(101);
data_rom.(x25+16) <- val_long(116);
data_rom.(x25+17) <- val_long(116);
data_rom.(x25+18) <- val_long(101);
data_rom.(x25+19) <- val_long(40);
data_rom.(x25+20) <- val_long(82);
data_rom.(x25+21) <- val_long(101);
data_rom.(x25+22) <- val_long(112);
data_rom.(x25+23) <- val_long(41);
data_rom.(x25+24) <- val_long(40);
data_rom.(x25+25) <- val_long(65);
data_rom.(x25+26) <- val_long(102);
data_rom.(x25+27) <- val_long(102);
data_rom.(x25+28) <- val_long(41);
data_rom.(x25+29) <- val_long(40);
data_rom.(x25+30) <- val_long(69);
data_rom.(x25+31) <- val_long(118);
data_rom.(x25+32) <- val_long(97);
data_rom.(x25+33) <- val_long(108);
data_rom.(x25+34) <- val_long(41);
data_rom.(x25+35) <- val_long(40);
data_rom.(x25+36) <- val_long(65);
data_rom.(x25+37) <- val_long(108);
data_rom.(x25+38) <- val_long(112);
data_rom.(x25+39) <- val_long(104);
data_rom.(x25+40) <- val_long(97);
data_rom.(x25+41) <- val_long(41);
data_rom.(x25+42) <- val_long(46);
data_rom.(x25+43) <- val_long(80);
data_rom.(x25+44) <- val_long(101);
data_rom.(x25+45) <- val_long(114);
data_rom.(x25+46) <- val_long(100);
data_rom.(global_start + 67) <- val_ptr(x25);
(* ADD GLOBAL 68 *)
(* ========= *)
data_rom.(x26) <- val_long(make_header(252,45));
let x27 = x26+46 in
data_rom.(x26+1) <- val_long(80);
data_rom.(x26+2) <- val_long(52);
data_rom.(x26+3) <- val_long(95);
data_rom.(x26+4) <- val_long(116);
data_rom.(x26+5) <- val_long(111);
data_rom.(x26+6) <- val_long(117);
data_rom.(x26+7) <- val_long(116);
data_rom.(x26+8) <- val_long(46);
data_rom.(x26+9) <- val_long(70);
data_rom.(x26+10) <- val_long(83);
data_rom.(x26+11) <- val_long(113);
data_rom.(x26+12) <- val_long(117);
data_rom.(x26+13) <- val_long(101);
data_rom.(x26+14) <- val_long(108);
data_rom.(x26+15) <- val_long(101);
data_rom.(x26+16) <- val_long(116);
data_rom.(x26+17) <- val_long(116);
data_rom.(x26+18) <- val_long(101);
data_rom.(x26+19) <- val_long(40);
data_rom.(x26+20) <- val_long(82);
data_rom.(x26+21) <- val_long(101);
data_rom.(x26+22) <- val_long(112);
data_rom.(x26+23) <- val_long(41);
data_rom.(x26+24) <- val_long(40);
data_rom.(x26+25) <- val_long(65);
data_rom.(x26+26) <- val_long(102);
data_rom.(x26+27) <- val_long(102);
data_rom.(x26+28) <- val_long(41);
data_rom.(x26+29) <- val_long(40);
data_rom.(x26+30) <- val_long(69);
data_rom.(x26+31) <- val_long(118);
data_rom.(x26+32) <- val_long(97);
data_rom.(x26+33) <- val_long(108);
data_rom.(x26+34) <- val_long(41);
data_rom.(x26+35) <- val_long(40);
data_rom.(x26+36) <- val_long(65);
data_rom.(x26+37) <- val_long(108);
data_rom.(x26+38) <- val_long(112);
data_rom.(x26+39) <- val_long(104);
data_rom.(x26+40) <- val_long(97);
data_rom.(x26+41) <- val_long(41);
data_rom.(x26+42) <- val_long(46);
data_rom.(x26+43) <- val_long(78);
data_rom.(x26+44) <- val_long(117);
data_rom.(x26+45) <- val_long(108);
data_rom.(global_start + 68) <- val_ptr(x26);
(* ADD GLOBAL 69 *)
data_rom.(global_start + 69) <- val_long(0);
(* ADD GLOBAL 70 *)
data_rom.(global_start + 70) <- val_long(0);
(* ADD GLOBAL 71 *)
(* ========= *)
data_rom.(x27) <- val_long(make_header(252,0));
let x28 = x27+1 in
data_rom.(global_start + 71) <- val_ptr(x27);
(* ADD GLOBAL 72 *)
(* ========= *)
data_rom.(x28) <- val_long(make_header(252,17));
let x29 = x28+18 in
data_rom.(x28+1) <- val_long(65);
data_rom.(x28+2) <- val_long(66);
data_rom.(x28+3) <- val_long(58);
data_rom.(x28+4) <- val_long(32);
data_rom.(x28+5) <- val_long(108);
data_rom.(x28+6) <- val_long(111);
data_rom.(x28+7) <- val_long(110);
data_rom.(x28+8) <- val_long(103);
data_rom.(x28+9) <- val_long(117);
data_rom.(x28+10) <- val_long(101);
data_rom.(x28+11) <- val_long(117);
data_rom.(x28+12) <- val_long(114);
data_rom.(x28+13) <- val_long(32);
data_rom.(x28+14) <- val_long(100);
data_rom.(x28+15) <- val_long(105);
data_rom.(x28+16) <- val_long(102);
data_rom.(x28+17) <- val_long(102);
data_rom.(global_start + 72) <- val_ptr(x28);
(* ADD GLOBAL 73 *)
data_rom.(global_start + 73) <- val_long(0);
(* ADD GLOBAL 74 *)
data_rom.(global_start + 74) <- val_long(0);
(* ADD GLOBAL 75 *)
data_rom.(global_start + 75) <- val_long(0);
(* ADD GLOBAL 76 *)
data_rom.(global_start + 76) <- val_long(0);
(* ADD GLOBAL 77 *)
data_rom.(global_start + 77) <- val_long(0);
(* ADD GLOBAL 78 *)
(* ========= *)
data_rom.(x29) <- val_long(make_header(252,43));
let x30 = x29+44 in
data_rom.(x29+1) <- val_long(80);
data_rom.(x29+2) <- val_long(52);
data_rom.(x29+3) <- val_long(95);
data_rom.(x29+4) <- val_long(116);
data_rom.(x29+5) <- val_long(111);
data_rom.(x29+6) <- val_long(117);
data_rom.(x29+7) <- val_long(116);
data_rom.(x29+8) <- val_long(46);
data_rom.(x29+9) <- val_long(70);
data_rom.(x29+10) <- val_long(65);
data_rom.(x29+11) <- val_long(108);
data_rom.(x29+12) <- val_long(112);
data_rom.(x29+13) <- val_long(104);
data_rom.(x29+14) <- val_long(97);
data_rom.(x29+15) <- val_long(98);
data_rom.(x29+16) <- val_long(101);
data_rom.(x29+17) <- val_long(116);
data_rom.(x29+18) <- val_long(97);
data_rom.(x29+19) <- val_long(79);
data_rom.(x29+20) <- val_long(40);
data_rom.(x29+21) <- val_long(82);
data_rom.(x29+22) <- val_long(101);
data_rom.(x29+23) <- val_long(112);
data_rom.(x29+24) <- val_long(41);
data_rom.(x29+25) <- val_long(40);
data_rom.(x29+26) <- val_long(69);
data_rom.(x29+27) <- val_long(118);
data_rom.(x29+28) <- val_long(97);
data_rom.(x29+29) <- val_long(108);
data_rom.(x29+30) <- val_long(41);
data_rom.(x29+31) <- val_long(46);
data_rom.(x29+32) <- val_long(65);
data_rom.(x29+33) <- val_long(108);
data_rom.(x29+34) <- val_long(112);
data_rom.(x29+35) <- val_long(104);
data_rom.(x29+36) <- val_long(97);
data_rom.(x29+37) <- val_long(67);
data_rom.(x29+38) <- val_long(111);
data_rom.(x29+39) <- val_long(117);
data_rom.(x29+40) <- val_long(112);
data_rom.(x29+41) <- val_long(117);
data_rom.(x29+42) <- val_long(114);
data_rom.(x29+43) <- val_long(101);
data_rom.(global_start + 78) <- val_ptr(x29);
(* ADD GLOBAL 79 *)
(* ========= *)
data_rom.(x30) <- val_long(make_header(252,42));
let x31 = x30+43 in
data_rom.(x30+1) <- val_long(80);
data_rom.(x30+2) <- val_long(52);
data_rom.(x30+3) <- val_long(95);
data_rom.(x30+4) <- val_long(116);
data_rom.(x30+5) <- val_long(111);
data_rom.(x30+6) <- val_long(117);
data_rom.(x30+7) <- val_long(116);
data_rom.(x30+8) <- val_long(46);
data_rom.(x30+9) <- val_long(70);
data_rom.(x30+10) <- val_long(65);
data_rom.(x30+11) <- val_long(108);
data_rom.(x30+12) <- val_long(112);
data_rom.(x30+13) <- val_long(104);
data_rom.(x30+14) <- val_long(97);
data_rom.(x30+15) <- val_long(98);
data_rom.(x30+16) <- val_long(101);
data_rom.(x30+17) <- val_long(116);
data_rom.(x30+18) <- val_long(97);
data_rom.(x30+19) <- val_long(79);
data_rom.(x30+20) <- val_long(40);
data_rom.(x30+21) <- val_long(82);
data_rom.(x30+22) <- val_long(101);
data_rom.(x30+23) <- val_long(112);
data_rom.(x30+24) <- val_long(41);
data_rom.(x30+25) <- val_long(40);
data_rom.(x30+26) <- val_long(69);
data_rom.(x30+27) <- val_long(118);
data_rom.(x30+28) <- val_long(97);
data_rom.(x30+29) <- val_long(108);
data_rom.(x30+30) <- val_long(41);
data_rom.(x30+31) <- val_long(46);
data_rom.(x30+32) <- val_long(66);
data_rom.(x30+33) <- val_long(101);
data_rom.(x30+34) <- val_long(116);
data_rom.(x30+35) <- val_long(97);
data_rom.(x30+36) <- val_long(67);
data_rom.(x30+37) <- val_long(111);
data_rom.(x30+38) <- val_long(117);
data_rom.(x30+39) <- val_long(112);
data_rom.(x30+40) <- val_long(117);
data_rom.(x30+41) <- val_long(114);
data_rom.(x30+42) <- val_long(101);
data_rom.(global_start + 79) <- val_ptr(x30);
(* ADD GLOBAL 80 *)
(* ========= *)
data_rom.(x31) <- val_long(make_header(252,0));
let x32 = x31+1 in
data_rom.(global_start + 80) <- val_ptr(x31);
(* ADD GLOBAL 81 *)
(* ========= *)
data_rom.(x32) <- val_long(make_header(0,2));
let x33 = x32+3 in
data_rom.(x32+1) <- val_long(0);
(* ========= *)
data_rom.(x33) <- val_long(make_header(0,2));
let x34 = x33+3 in
data_rom.(x33+1) <- val_long(2);
(* ========= *)
data_rom.(x34) <- val_long(make_header(0,2));
let x35 = x34+3 in
data_rom.(x34+1) <- val_long(10);
(* ========= *)
data_rom.(x35) <- val_long(make_header(0,2));
let x36 = x35+3 in
data_rom.(x35+1) <- val_long(50);
data_rom.(x35+2) <- val_long(0);
data_rom.(x34+2) <- val_ptr(x35);
data_rom.(x33+2) <- val_ptr(x34);
data_rom.(x32+2) <- val_ptr(x33);
data_rom.(global_start + 81) <- val_ptr(x32);
(* ADD GLOBAL 82 *)
data_rom.(global_start + 82) <- val_long(0);
(* ADD GLOBAL 83 *)
(* ========= *)
data_rom.(x36) <- val_long(make_header(252,22));
let x37 = x36+23 in
data_rom.(x36+1) <- val_long(80);
data_rom.(x36+2) <- val_long(52);
data_rom.(x36+3) <- val_long(95);
data_rom.(x36+4) <- val_long(116);
data_rom.(x36+5) <- val_long(111);
data_rom.(x36+6) <- val_long(117);
data_rom.(x36+7) <- val_long(116);
data_rom.(x36+8) <- val_long(46);
data_rom.(x36+9) <- val_long(80);
data_rom.(x36+10) <- val_long(52);
data_rom.(x36+11) <- val_long(95);
data_rom.(x36+12) <- val_long(101);
data_rom.(x36+13) <- val_long(118);
data_rom.(x36+14) <- val_long(97);
data_rom.(x36+15) <- val_long(108);
data_rom.(x36+16) <- val_long(46);
data_rom.(x36+17) <- val_long(81);
data_rom.(x36+18) <- val_long(117);
data_rom.(x36+19) <- val_long(97);
data_rom.(x36+20) <- val_long(116);
data_rom.(x36+21) <- val_long(114);
data_rom.(x36+22) <- val_long(101);
data_rom.(global_start + 83) <- val_ptr(x36);
(* ADD GLOBAL 84 *)
(* ========= *)
data_rom.(x37) <- val_long(make_header(252,28));
let x38 = x37+29 in
data_rom.(x37+1) <- val_long(80);
data_rom.(x37+2) <- val_long(52);
data_rom.(x37+3) <- val_long(95);
data_rom.(x37+4) <- val_long(116);
data_rom.(x37+5) <- val_long(111);
data_rom.(x37+6) <- val_long(117);
data_rom.(x37+7) <- val_long(116);
data_rom.(x37+8) <- val_long(46);
data_rom.(x37+9) <- val_long(80);
data_rom.(x37+10) <- val_long(52);
data_rom.(x37+11) <- val_long(95);
data_rom.(x37+12) <- val_long(101);
data_rom.(x37+13) <- val_long(118);
data_rom.(x37+14) <- val_long(97);
data_rom.(x37+15) <- val_long(108);
data_rom.(x37+16) <- val_long(46);
data_rom.(x37+17) <- val_long(86);
data_rom.(x37+18) <- val_long(97);
data_rom.(x37+19) <- val_long(108);
data_rom.(x37+20) <- val_long(101);
data_rom.(x37+21) <- val_long(117);
data_rom.(x37+22) <- val_long(114);
data_rom.(x37+23) <- val_long(95);
data_rom.(x37+24) <- val_long(110);
data_rom.(x37+25) <- val_long(117);
data_rom.(x37+26) <- val_long(108);
data_rom.(x37+27) <- val_long(108);
data_rom.(x37+28) <- val_long(101);
data_rom.(global_start + 84) <- val_ptr(x37);
(* ADD GLOBAL 85 *)
(* ========= *)
data_rom.(x38) <- val_long(make_header(252,27));
let x39 = x38+28 in
data_rom.(x38+1) <- val_long(80);
data_rom.(x38+2) <- val_long(52);
data_rom.(x38+3) <- val_long(95);
data_rom.(x38+4) <- val_long(116);
data_rom.(x38+5) <- val_long(111);
data_rom.(x38+6) <- val_long(117);
data_rom.(x38+7) <- val_long(116);
data_rom.(x38+8) <- val_long(46);
data_rom.(x38+9) <- val_long(80);
data_rom.(x38+10) <- val_long(52);
data_rom.(x38+11) <- val_long(95);
data_rom.(x38+12) <- val_long(101);
data_rom.(x38+13) <- val_long(118);
data_rom.(x38+14) <- val_long(97);
data_rom.(x38+15) <- val_long(108);
data_rom.(x38+16) <- val_long(46);
data_rom.(x38+17) <- val_long(65);
data_rom.(x38+18) <- val_long(114);
data_rom.(x38+19) <- val_long(103);
data_rom.(x38+20) <- val_long(95);
data_rom.(x38+21) <- val_long(105);
data_rom.(x38+22) <- val_long(110);
data_rom.(x38+23) <- val_long(118);
data_rom.(x38+24) <- val_long(97);
data_rom.(x38+25) <- val_long(108);
data_rom.(x38+26) <- val_long(105);
data_rom.(x38+27) <- val_long(100);
data_rom.(global_start + 85) <- val_ptr(x38);
(* ADD GLOBAL 86 *)
(* ========= *)
data_rom.(x39) <- val_long(make_header(252,20));
let x40 = x39+21 in
data_rom.(x39+1) <- val_long(80);
data_rom.(x39+2) <- val_long(52);
data_rom.(x39+3) <- val_long(95);
data_rom.(x39+4) <- val_long(116);
data_rom.(x39+5) <- val_long(111);
data_rom.(x39+6) <- val_long(117);
data_rom.(x39+7) <- val_long(116);
data_rom.(x39+8) <- val_long(46);
data_rom.(x39+9) <- val_long(80);
data_rom.(x39+10) <- val_long(52);
data_rom.(x39+11) <- val_long(95);
data_rom.(x39+12) <- val_long(116);
data_rom.(x39+13) <- val_long(101);
data_rom.(x39+14) <- val_long(120);
data_rom.(x39+15) <- val_long(116);
data_rom.(x39+16) <- val_long(46);
data_rom.(x39+17) <- val_long(67);
data_rom.(x39+18) <- val_long(111);
data_rom.(x39+19) <- val_long(117);
data_rom.(x39+20) <- val_long(112);
data_rom.(global_start + 86) <- val_ptr(x39);
  for i = 0 to 2047 do
    ram.(i) <- data_rom.(i)
  done ) ;;
let main_load () =
  load_code();
  init_data();
  global_end.(0) <- global_start + 87 ;;


type others = bool * bool ;;


type state = (short * value * short * (value * char * short) * others) ;;

let caml_set_finish (_,led) =
  (true,led) ;;

let others_default = (false, false) ;;


(* ********************************************** *)


let step_vm (s : state) : state =
  let (pc, acc, sp, other_regs, others) = s in
  let (env, extra_args, trap_sp) = other_regs in
  print_string "pc:";   print_int pc;
  (* print_string "(opcode:";   print_int (code[pc]); print_string ")"; *)
  print_string "|acc:"; print_val acc;
  print_string "|sp:";  print_int sp;
  print_string "|env:";  print_val env;
  (*print_string "stack:"; print_stack(sp);*)
  print_newline ();

  (*  ************* precomputed values  ************* *)

  let pc_plus_1 : short = pc + 1 in

  let apply((i1,i2,i3),next_extra_args,appterm,n,ofs) =
    let dont_care = val_unit in
    let (arg1,sp) = if i1 then pop_stack(sp) else (dont_care,sp) in
    let (arg2,sp) = if i2 then pop_stack(sp) else (dont_care,sp) in
    let (arg3,sp) =  (if i3 then pop_stack(sp) else (dont_care,sp)) in


    let sp = push_stack(val_long(long_of_char(extra_args)),sp) in
    let sp = push_stack(env,sp) in
    let sp = push_stack(val_long(as_long(pc_plus_1)),sp) in

    let sp = if i3 then push_stack(arg3,sp) else sp in
    let sp = if i2 then push_stack(arg2,sp) else sp in
    let sp =  (if i1 then push_stack(arg1,sp) else sp) in
    let next_pc = as_short(long_val (get_field(acc,0))) in
    let next_env = acc in
    (next_pc,acc,sp,(next_env, next_extra_args, trap_sp), others)
  in


  (* ************* DECOD: 1 cycle **************** *)
  (* assert (pc < code.length);*)
  let x = code.(pc) 
  and (popped,sp_minus_1) = pop_stack(sp) in
  (* ************* EXEC ************************** *)
  match x with
  | GROUP1(i,n) ->
      let sp_minus_n = sp - n in
      let sp_minus_n_minus_1 = sp_minus_n - 1 in
      let (next_acc,next_sp) =
        match i with
        | ACC() -> (* 1 cycle *)
            let v = stack_get(sp_minus_n_minus_1) in
            (v,sp)
        | PUSH() -> (* 1 cycle *)
            let sp = push_stack(acc,sp) in
            (acc, sp)
        | POP() -> (* 0 cycle *)
            (acc, sp_minus_n)
        | ASSIGN() -> (* 1 cycle *)
              stack_set(sp_minus_n_minus_1, acc);
              (val_unit,sp)
        | ENVACC() -> (* 1 cycle *)
            let v = get_field(env,n) in
            (v, sp)
        | CONST(n) -> (* 0 cycle *) 
            (val_long n, sp)
        | OFFSET(ofs) -> (* 0 cycle *) 
            let res = addint(long_val acc, ofs) in 
            (val_long(res), sp)
        | OFFSETREF(ofs) -> 
            let f0 = get_field0(acc) in (* 1 cycle *) 
            let v = val_long((long_val f0) + ofs) in
            set_field0(acc,v);
            (val_unit,sp)
        | GETFIELD() -> (* 1 cycle *) 
            assert (is_ptr(acc));
            let v = get_field(acc,n) in
            (v,sp)
        (*| GETGLOBALFIELD(p) -> (* 2 cycle *) 
            let u = global_get(n) in 
            let v = get_field(u,p) in
            (v,sp)*)
        | OFFSETCLOSURE() -> (* 0 cycle *)    
             print_block (env); print_string "===============%%>\n"; print_int n;
            let v = val_ptr (ptr_val(env) + n*2) in
            (v,sp)
        | PUSHRETADDR() -> (* 3 cycles *)
            let sp = push_stack(val_long(long_of_char(extra_args)),sp) in
            let c = val_long (as_long n) in
            let sp = push_stack(env,sp) in
            let sp = push_stack(c,sp) in
            (acc,sp)
        | GETGLOBAL() -> (* 1 cycles *)
            let v = global_get n in
            (v, sp)
        | SETGLOBAL(v) -> (* 1 cycles *)
            global_set(n,acc);
            (val_unit, sp)
         | UNOP(op) ->
            let res = match op with 
                      | NOT() ->  bnot (long_val (acc))
                      | NEG() ->  - (long_val (acc))
                      (*| OFFSET(ofs) -> addint(long_val acc, ofs)*)
                      | VECTLENGTH() -> as_long(size_val acc)
                      | ISINT() -> int_of_bool(is_int(acc))
                      | _ -> 0 
                       in
            (val_long(res), sp)

        | _ -> print_string "unknown opcode : ";
           (* print_int (code[pc]);*)
           print_newline ();
           (acc,sp)
      
    in
    (pc_plus_1,next_acc, next_sp, other_regs, others)
  | GROUP2(i) ->
      let binop(op) = 
        let a1 = long_val acc in
        let a2 = long_val(popped) in
        let res = op(a1,a2) in
        let v = val_long res in
        (v, sp_minus_1)
      in
      let (next_acc,next_sp)= 
       (match i with
        | ADD() -> (* 0 cycle *)
            binop(addint)
        | SUB() -> (* 0 cycle *)
            binop(subint)
        | MUL() ->
            binop(mulint)
        | DIV() ->
            binop(divint)
        | MOD() ->
            binop(modint)
        | OR() -> (* 0 cycle *)
            binop(orint)
        | AND() -> (* 0 cycle *)
            binop(andint)
        | LSL() -> 
            binop(lslint)
        | ASR() -> 
            binop(lsrint)
          (*| MOD() -> binop(modint)
         | DIV() -> binop(modint)*)
        | GETVECTITEM() -> (* 1 cycle *) 
            let (n,sp) = (popped,sp_minus_1) in
            let v = get_field(acc,as_short(long_val(n))) in
            (v, sp)
        | SETVECTITEM() -> (* 2 cycles *) 
            let (n,sp) = (popped,sp_minus_1) in
            let (v,sp) = pop_stack(sp) in
            set_field(acc,as_short(long_val(n)),v);
            let next_acc = val_unit in
            (next_acc,sp)

        | COMPARE(op) -> (* 0 cycle *)
            let a1 = long_val acc in
            let v2 = popped in
            let a2 = long_val(v2) in
            let v1 = a1 < a2 in
            let v2 = a1 = a2 in
            let res = match op with
                      | LT() -> v1
                      | LE() -> v1 or v2
                      | EQ() -> v2
                      | GE() -> not(v1)
                      | GT() -> not(v1 or v2)
                      | NEQ() -> not(v2)
                      | _ -> false
                       in
            let v = val_long (int_of_bool res) in
            (v,sp_minus_1)

        | SETFIELD(n) -> (* 1 cycle *)    
            set_field(acc,n,popped);
            (val_unit, sp_minus_1)

        | _ -> 
            print_string "unknown opcode : ";
            (* print_int (code[pc]);*)
            print_newline ();
            (acc,sp)
    
        ) 
      in
      (pc_plus_1,next_acc, next_sp, other_regs, others)

  | GROUP3(i) ->  
      let (next_pc,others) = 
       (match i with
        | BRANCH(l) -> (* 0 cycle *)
            (l,others)
        | BRANCHIF(b,l) -> (* 0 cycle *) 
            let l2 = if bxor(b, (is_long acc & long_val(acc) = 0)) then l else pc_plus_1 in
            (l2,others)
  
        | BCOMPARE(op,n,l) -> (* 0 cycle *)
            let v = long_val acc in
            let v1 = n < v in
            let v2 = n = v in
            let res = match op with
                      | LT() -> v1
                      | LE() -> v1 or v2
                      | EQ() -> v2
                      | GE() -> not(v1)
                      | GT() -> not(v1 or v2)
                      | NEQ() -> not(v2) in
            let l2 = if res then l else pc_plus_1 in
            (l2,others)
        | STOP() -> (* 0 cycle *)
            print_string "STOP : "; 
            (pc, caml_set_finish(others))
        | SWITCH(l1,l2) -> 
            let ofs = as_short(long_val acc) in
            let pc2 = (if (is_int acc) then l1 else l2) + ofs in
            (pc2,others)
        | CHECK_SIGNALS() -> (pc_plus_1,others)
        )
    in
    (next_pc, acc, sp, other_regs, others)   

         | LABEL(l) -> (* 0 cycle *)
            (l, acc, sp, other_regs, others)   


    | CALL(p,gt1,gt2,gt3,gt4) -> (* N cycles where N is the duration of p *)
        let (a1,sp) = if gt1 then (popped,sp_minus_1) else (val_unit,sp) in
        let (a2,sp) = if gt2 then pop_stack(sp) else (val_unit,sp) in
        let (a3,sp) = if gt3 then pop_stack(sp) else (val_unit,sp) in
        let (a4,sp) = if gt4 then pop_stack(sp) else (val_unit,sp) in
        let st = (pc,acc,sp, (env, extra_args, trap_sp), others) in
        let (res,st) = external_call (p,(acc,(a1,(a2,(a3,(a4))))),st) in
        let (pc,acc,sp, (_, extra_args, trap_sp), others) = st in (* must not modify env *)
        let acc = res in
        (pc_plus_1,acc,sp, (env, extra_args, trap_sp), others)

    | GROUP4(i) -> 
       (match i with
        | POPTRAP() -> (* 1 cycle *)
            let sp = sp - 1 in
            let (v,sp) = pop_stack(sp) in
            let next_trap_sp = as_short(long_val(v)) in
            let sp = sp - 2 in
            (pc_plus_1,acc,sp,(env, extra_args, next_trap_sp), others)
        | RAISE() -> (* 4 cycle *)
            (* if trap_sp = (-1)   (* i.e., uncaught_exception *)
            then (143,acc,sp, other_regs, others) else *)
            let sp = trap_sp in
            let (v0,sp) = pop_stack(sp) in
            let next_pc = as_short(long_val(v0)) in
            let (v1,sp) = pop_stack(sp) in
            let next_trap_sp = as_short(long_val(v1)) in
            let (v2,sp) = pop_stack(sp) in
            let next_env = v2 in
            let (v3,sp) = pop_stack(sp) in
            let next_extra_args = char_of_long(long_val(v3)) in
            (next_pc,acc,sp, (next_env, next_extra_args, next_trap_sp), others)
        | RESTART() -> (* (nbargs+1)+1 cycles *)
            let nbargs = char_of_short(size_val(env)) - 2 in
            print_string "=======GGGGGGG;"; print_int (size_val(env));
            let rec loop_push(sp,i) =
              if pause (i >= nbargs) then sp else
              let sp = push_stack(get_field(env,short_of_char(i+2)),sp) in
              loop_push(sp,i+1)
            in
            let sp = loop_push(sp,0) in
            let next_env = get_field1(env) in
            let next_extra_args = extra_args + nbargs in
            (pc_plus_1,acc,sp, (next_env, next_extra_args, trap_sp), others)
      )

        | MAKEBLOCK(atom,closure,sz,tag,l) ->
                  (*let (acc,env,blk) = make_block(sp,acc,env,tag,sz) in
                  (* if atom then (pc_plus_1,blk,sp,(env, extra_args, trap_sp), others) else *)(
                  set_field0(blk,acc);
                  let rec fill(i,sp) =
                     if i = 0 then sp else
                     let (v,sp) = pop_stack(sp) in
                     (set_field(blk,i,v); fill(i-1,sp))
                  in
                  let sp = fill(sz,sp) in
                  (pc_plus_1,blk,sp,(env, extra_args, trap_sp), others))*)
            let u = closure & sz > 0 in
            (* print_string "CLOSURE: "; print_int (if u then 1 else 0); *)
            let sp = pause(if u then push_stack(acc,sp) else sp) in
            let sz_block = if closure then sz+1 else sz in
            let (acc,env,blk) = make_block(sp,acc,env,tag,sz_block) in
            let v = if closure then val_int (as_long(l)) else acc in
            set_field0(blk,v);
            if atom then (pc_plus_1,blk,sp,(env, extra_args, trap_sp), others) else (
                            let rec fill(i,sp) = 
                              if i >= (if closure then sz+1 else sz) then sp else
                              (let (v,sp) = pop_stack(sp) in
                               set_field(blk,i,v); fill(i+1,sp))
                            in
                            let sp = fill(1,sp) in
      print_block(blk);
      (pc_plus_1,blk,sp,(env, extra_args, trap_sp), others))


 | RETURN(n) -> (* 3 cycles if (extra_args = 0) else 1 cycle *)
       let sp = sp - n in
          print_int (sp);
       let u = pause (extra_args = 0) in 
       if u then (
         let then_next_pc = as_short(long_val(stack_get(sp-1))) in
         let next_env =                              stack_get(sp-2)   in
         let next_extra_args = char_of_long(long_val(stack_get(sp-3))) in
         (then_next_pc,acc,sp-3,(next_env, next_extra_args, trap_sp), others)
         )
       else (
         (* 1 cycle *) 
         let next_env = acc in
         let next_extra_args = extra_args - 1 in
         let next_pc = as_short(long_val(get_field0(acc))) in
         (next_pc, acc, sp,(next_env, next_extra_args, trap_sp), others))

 | APPTERM(n,s) -> 
      let n8 = char_of_short(n) in
      let next_sp = sp - s + n in
      let rec w i =
     
        if i = 0 then () else (
          stack_set(next_sp-i, stack_get(sp-i)); w(i-1)
        )
      in w(n);
      let next_pc = as_short(long_val(get_field0(acc))) in
      let next_env = acc in
      let next_extra_args = extra_args + n8 - 1 in
      (next_pc,acc,next_sp,(next_env, next_extra_args, trap_sp), others)
 | APPLY(special,c,n) -> (* 1 cycle *)
      if special then apply ( (
             match c with 
             | 1 -> ((true,false,false),0,false,0,0)
             | 2 -> ((true,true,false),1,false,0,0)
             | _ (* 3 *) -> ((true,true,true),2,false,0,0) 
           ))
     else
          let next_extra_args = c - 1 in
          let next_env = acc in
          let next_pc_apply = as_short(long_val(get_field0(acc))) in
          (next_pc_apply,acc,sp,(next_env, next_extra_args, trap_sp), others)

  | PUSHTRAP(l) ->
       let sp = push_stack(val_long(long_of_char(extra_args)),sp) in
       let sp = push_stack(env,sp) in
       let c = val_long(as_long(l)) in
       let sp = push_stack(val_long(as_long(trap_sp)),sp) in
       let sp = push_stack(c,sp) in
       let next_trap_sp = sp in
       (pc_plus_1, acc, sp,(env, extra_args, next_trap_sp), others)


| GRAB(c) ->     
    let x = extra_args in
    let u = pause (x >= c) in
    if u then let next_extra_args = x - c in
                   (pc_plus_1,acc,sp, (env, next_extra_args, trap_sp), others)
    else (
     let (_,env,next_acc) = make_block(sp,acc,env,closure_tag,short_of_char(x + 3)) in
     (* print_string "%%%%%%%%%%%%%%%%%%%%%%%%%%%%=>"; print_int (pc - 1);*) (* minus 2 ? *)
     set_field0(next_acc, val_long (as_long(pc - 1)));
     set_field1(next_acc, env);
     let rec loop(i,sp) =       (* todo *)
       if i > x then sp else
       let (v,sp) = pop_stack(sp) in
       set_field(next_acc,short_of_char(i+2),v);
       loop(i+1,sp)
     in
     let sp = loop(0,sp) in
     let v = stack_get(sp-1) in
     let next_pc = as_short(long_val v) in
     let w = stack_get(sp-2) in
     let next_env = w in
     let u = stack_get(sp-3) in
     let next_extra_args = char_of_long(long_val(u)) in
     (next_pc,next_acc,sp-3, (next_env, next_extra_args, trap_sp), others))

| CLOSUREREC(f,v,o,l) ->
       let f = short_of_char(f) in
       let sp = if v > 0 then push_stack(acc,sp) else sp in
       let closure_size = (2 * f) - 1 + v in
       let (_,env,next_acc) = make_block(sp,acc,env,closure_tag,closure_size) in
       set_field(next_acc, 0, val_long (as_long(l)));
       let rec w0(i,sp) =
         if i = v (* >= *) then sp else
         let (x,sp) = pop_stack(sp) in
         (set_field(next_acc, i + 2 * f - 1, x); w0(i+1,sp))
       in 
       let sp = w0(0,sp) in
       let rec w1(i) =
         if i >= f then () else
         (((* if i <> f-1 then *)
          set_field(next_acc,2*i-1,val_long(make_header(infix_tag,closure_size-2*i)));
          set_field(next_acc,2*i,val_long(as_long(o+i))); w1(i+1)))
       in
       w1(1);
       (* print_block next_acc; *)
       let sp = push_stack(next_acc,sp) in
       let rec w3(i,sp) =
         if i >= f then sp else
         let sp = push_stack(val_long (as_long (ptr_val next_acc + (2 * i))),sp) in
         w3(i+1,sp)
       in
       print_block(next_acc);
       let sp = w3(1,sp) in
       (pc_plus_1, next_acc, sp,(env, extra_args, trap_sp), others)


| _ -> s  ;;
let nb_step = 
  if Array.length Sys.argv < 2 then max_int (* "no limit" (2^31) steps *)
  else int_of_string @@ Sys.argv.(1) ;;

let is_finished (pc,_,_,_,_) = 
  match code.(pc) with
  | GROUP3(STOP()) -> true
  | _ -> false

let rec loop (i,s) =
  if i < 0 then s else 
  let s' = step_vm (s) in
  if is_finished s' then step_vm(s')
  else 
    loop (i-1,s') ;; 
  
let _ =
  stack_set(0, val_unit);
  main_load();
  from_space_array.(0) <- heap_start;
  to_space_array.(0) <- heap_start + heap_size;
  next_array.(0) <- heap_start;

  loop (nb_step,(0,val_unit,stack_start,(val_unit, 0, 0),others_default));;
