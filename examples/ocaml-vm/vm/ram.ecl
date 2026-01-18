
let stack_start : short = 1;;
let heap_start : short = 2048 ;;
let heap_size : short = 13312 (* 8192*) ;;

let global_start : short = 0 ;;

(* the end of the section containing the OCaml global,
   global_end[0] is set during loading the bytecode *)

(* [rec] annotation introduces a pause, augmenting the throughput, but potentially
   breaking the critical path and diminishing the area of the generated hardware *)
let ram_get (i) = ram.(i) ;;
let ram_set (i,v) = ram.(i) <- v ;;

let global_get n =
  ram.(global_start + n) ;;

let global_set ((n,v) : (short * value)) =
  ram.(global_start + n) <- v ;;



let stack_get i = stack.(i) ;;
let stack_set (i,v) = stack.(i) <- v ;;
let pop_stack (sp) =
  (* assert (sp[0] > 0); *)
  let p = sp - 1 in
  let v = stack_get(p) in
  (v,p) ;;
let push_stack ((v,sp) : (value * short)) : short =
  let sp_plus_1 = sp + 1 in
  stack_set(sp, v);
  sp_plus_1 ;;

let imm_push_stack ((v,sp) : (value * short)) : short =
  let sp_plus_1 = sp + 1 in
  immediate set(stack,sp, v);
  sp_plus_1 ;;

let get_field(v,i) =
  ram_get(ptr_val(v) + i + 1) ;;

let set_field(v,i,w) =
  ram_set(ptr_val(v) + i + 1,  w) ;;

let imm_set_field(v,i,w) =
  immediate set(ram, ptr_val(v) + i + 1,  w) ;;

let get_field0(v) = get_field(v,0) ;; (*
  ram_get(ptr_val(v) + 1) ;;*)

let get_field1(v) = get_field(v,1) ;; (*
  ram_get(ptr_val(v) + 2) ;;*)

let set_field0(v,w) = set_field(v,0,w) ;; (*
  ram_set(ptr_val(v) + 1,  w) ;;*)

let set_field1(v,w) = set_field(v,1,w) ;; (*
  ram_set(ptr_val(v) + 2,  w) ;;*)

let get_stack_ofs(sp,i) =
  stack_get(sp - i - 1);;
