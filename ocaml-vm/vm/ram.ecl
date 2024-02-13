

let static ram = (0,true)^16384 ;;

let stack_start : short = 1000;;
let heap_start : short = 4000 ;;
let heap_size : short = 6000 ;;

let data_start : short = 0 ;;
let global_start : short = 16000 ;;

(* the end of the section containing the OCaml global,
   global_end[0] is set during loading the bytecode *)
let static global_end = 0^1 ;;

let stack_size () : short =
  heap_start - stack_start ;;

(* [rec] annotation introduces a pause, augmenting the throughput, but potentially
   breaking the critical path and diminishing the area of the generated hardware *)
let (*rec*) ram_get (i) = ram[i] ;;
let (*rec*) ram_set (i,v) = ram[i] <- v ;;

let push_stack ((v,sp) : (value * short)) : short =
  let sp_plus_1 = sp + 1 in
  ram_set(sp, v);
  sp_plus_1 ;;

let pop_stack (sp) =
  (* assert (sp[0] > 0); *)
  let p = sp - 1 in
  let v = ram_get(p) in
  (v,p) ;;

let get_field(v,i) =
  ram_get(ptr_val(v) + i + 1) ;;

let set_field(v,i,w) =
  ram_set(ptr_val(v) + i + 1,  w) ;;

let get_stack_ofs(sp,i) =
  ram_get(sp - i - 1);;

let global_get n =
  ram_get(global_start + n) ;;

let global_set ((n,v) : (short * value)) =
  ram_set(global_start + n, v) ;;

