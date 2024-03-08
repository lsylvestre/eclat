
let no_scan_tag : char = 251 ;;
let string_tag : char = 252 ;;
let closure_tag : char = 247 ;;
let infix_tag : char = 249 ;;
let fwd_ptr_tag : char = 248 ;;


let size_hd hd =
  (* [as_short] (i.e. resize) is used to forget higher bits (tag) *)
  as_short (hd lsr 2) ;;

let size (ptr:short) : short =
  let hd = ram[ptr] in
  size_hd (as_long(ptr_val(hd))) ;;

let tag ptr =
  let hd = ram[ptr] in
  (ptr_val(hd)) lsr 24 ;;

let size_val v = size (ptr_val v) ;;
let tag_val v : short = tag (ptr_val v) ;;

let make_header ((tag,sz) : (char * short)) : long =
  (long_of_char(tag) lsl 24) lor (as_long(sz) lsl 2) ;;


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
               start_space <= p && p < start_space + heap_size) ;;

(* [copy(from_space, to_space, v, next)] moves value [v]
   to the first available place [next] in [to_space] if [v] is a pointer in [from_space]. *)
let copy(from_space, to_space, v, next) =
  if not(is_pointer(from_space,v))
  then
    (v,next)
  else (
    let p = ptr_val v in
    let w = ram[p + 1] in
    if is_pointer(to_space,w) then
      (* [v] has already been copied in [to_space] at address [ptr_of_val(w)] *)
      (w,next)
    else (
      (* [v] must be copied in [to_space] *)
      let hd = ram[p] in
      let sz = size_hd (long_val(hd)) + 1(* +1? *) in

      print_string "bloc ";      print_int p ;
      print_string " of size ";  print_int sz;
      print_string " from ";     print_int (p);
      print_string " to ";       print_int next;  print_newline ();

      ram[next] <- hd;

      let rec loop(i) =
        if i >= sz then () else
        (ram[next+i] <- ram[p+i]; loop(i+1))
      in
      loop(1);
      ram[p] <- val_ptr(next); (* ? *)
      ram[p + 1] <- val_ptr(next);
      (val_ptr(next),next + sz)
    )
  )
;;



let stop_and_copy (sp,acc,env,from_space,to_space) =

 message_start_gc ();

 let next = to_space in

  (* ================ starting with roots: acc, env, stack, globals ================ *)
  let (next_acc,next) = copy (from_space, to_space, acc, next) in
  let (next_env,next) = copy (from_space, to_space, env, next) in

  let rec copy_root_in_ram(i,section_end,next) =
    if i >= section_end then next else
    (print_string "racine:"; print_int i; print_newline ();
    let (w,next) = copy (from_space, to_space, ram.(i), next) in
    ram.(i) <- w;
    print_string " next="; print_int next; print_newline ();
    copy_root_in_ram(i+1,section_end,next))
  in

  let next = copy_root_in_ram(stack_start,sp,next) in
  let next = copy_root_in_ram(global_start,global_end.(0),next) in

  message_gc_middle ();

  (* scan objects in to_space (including objects added by this loop) *)
  let rec aux(scan,next) =

    print_string "     scan="; print_int scan ;  print_string " | next="; print_int next; print_newline ();

    if scan >= next then next else
    let sz = size_hd (as_long (ptr_val (ram.(scan)))) + 1 in
    let rec loop(i,next) =
      (* print_string " ~~ scan"; print_newline (); *)
      if i >= sz then next else
      let scan_plus_i = scan + i in
      let (w,next) = copy (from_space, to_space, ram.(scan_plus_i),next) in
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

  if mem_copied > heap_size then fatal_error "Out of memory" else

  (next_acc,next_env,next)
;;

let gc(sp,acc,env,from_space, to_space) =
  stop_and_copy (sp,acc,env,from_space,to_space) ;;

(* =========================== ALLOC =========================== *)

let gc_alloc ((sp, acc, env,sz) : (short * value *value * short)) : (value * value * short) =
  print_string "GC-ALLOC:(size="; print_int sz; print_string ")"; print_newline ();
  let step((next_acc,next_env,p, next, from_space, to_space),_) =

    (* print_string "---------> "; print_int p;  print_string " / "; print_int next;  print_newline (); *)
      exec
        let next_plus_sz = next + sz in
        let from_space_plus_heap_size = from_space + heap_size in
      (* print_string "~~~~~~> "; print_int next_plus_sz;
         print_string "|~~~~~~> "; print_int from_space_plus_heap_size; print_newline (); *)
        if next_plus_sz > from_space_plus_heap_size
        then (
          let (next_acc,next_env,next) = gc(sp,acc,env,from_space, to_space) in

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

          (next_acc,next_env,next, next + sz, to_space, from_space)
        )
        else (acc, env,next, next_plus_sz, from_space, to_space)
      default (next_acc,next_env,p, next, from_space, to_space)
  in
  let rec wait () =
    let ((next_acc,next_env,p, next, from_space, to_space),rdy) =
      reg step last ((acc, env,heap_start, heap_start, heap_start, heap_start + heap_size),false)
    in
    if rdy then (next_acc,next_env,p) else wait ()
  in
  wait () ;;



(* =========================== MAKE_BLOCK =========================== *)

let rec make_block(sp, acc, env, tag,sz) : (value * value * value) =
  let sz = if sz = 0 then 1 else sz in
  let (next_acc,next_env, a) = gc_alloc (sp, acc, env,sz + 1) in
   print_string "size:"; print_int(sz); print_newline();
  let hd = (make_header(tag,sz)) in

  ram[a] <- val_int(hd);
  (* print_string "make_block:"; print_int(size_hd hd); print_newline();
  print_string "HEADER:"; print_int a; print_string "  "; print_int (long_val(ram[a])); print_newline (); *)
  (next_acc,next_env,val_ptr(a)) ;;

let make_closure ((sp,acc,env, pc,size) : (short * value * value * short * short)) : (value * value * value) =
  let (next_acc,next_env,res) = make_block(sp, acc,env, closure_tag,size) in
  set_field(res,0,val_int (as_long(pc)));
  (next_acc,next_env,res) ;;


