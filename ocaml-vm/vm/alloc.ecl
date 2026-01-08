
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
let rec copy(from_space, to_space, v, next) =
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

      immediate set(ram,next,hd);
      let rec loop(i) =
        if i = sz (* >= *) then () else
        (immediate set(ram,next+i,ram.(p+i));
         loop(i+1))
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
    immediate set(ram,i, w);
    print_string " next="; print_int next; print_newline ();
    copy_root_in_ram(i+1,section_end,next))
  in
  let next = copy_root_in_ram(global_start,global_end.(0),next) in  

  let rec copy_root_in_stack(i,section_end,next) =
    if i = section_end (* >= *) then next else
    (print_string "racine:"; print_int i; print_newline ();
    let (w,next) = (copy (from_space, to_space, stack.(i), next)) in
    immediate set(stack,i, w);
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
      immediate set(ram,scan_plus_i, w);
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
  (val_unit,val_unit,0) ;;

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
  (* print_string "ADDR-MAKE-block:"; print_int a; print_newline (); *)
  ram.(a) <- val_int(hd);
  (* print_block(val_ptr(a)); *)
  (* print_string "make_block:"; print_int(size_hd hd); print_newline();
     print_string "HEADER:"; print_int a; print_string "  "; print_int (long_val(ram[a])); print_newline (); *)
  (next_acc,next_env,val_ptr(a)) ;;

(*
let make_closure ((sp,acc,env, pc,size) : (short * value * value * short * short)) : (value * value * value) =
  let (next_acc,next_env,res) = make_block(sp, acc,env, closure_tag,size) in
  set_field(res,0,val_int (as_long(pc)));
  (next_acc,next_env,res) ;;
*)


