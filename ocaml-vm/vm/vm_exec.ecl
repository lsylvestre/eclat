
(* ********************************************** *)

let interp (go) =
  let exec_step (s,_) =
    if not(go) then (s,true)
    else (let (s2,rdy) = exec
                           step_vm s
                         default s
          in (s2,rdy))
  in
  reg exec_step last ((0,val_unit,stack_start,(val_unit, 0, 0),others_default),true) ;;

let config1 () =
  stack_set(0, val_unit);
  from_space_array.(0) <- heap_start;
  to_space_array.(0) <- heap_start + heap_size;
  next_array.(0) <- heap_start;
  caml_fresh.(0) <- 0;
  (* gnext[0] <- 0; *)
  (* from_space_array[0] <- 0;
  to_space_array[0] <- heap_size;*)
  main_load () (*;
  load_code ()*) ;;

let print_cy = true ;;


(** [await(i,rst)] sustains value true as soon as
    input [i] is true until [rst] is false *)
let await (i,rst) : bool =
  let step(s) = (s or i) & not rst in
  reg step last false ;;

let load_bytecode (rst) =
  let ((),rdy) = exec config1 () default () in
  await(rdy,rst) ;;

let counter () = reg (fun c -> c + 1) init 0;;

let ocaml_vm (button) =
  let step ((_,_,init_done,led),last_acc) =
    if not(init_done)
    then
      let rdy = load_bytecode(false) in
      ((false,true,rdy,led),last_acc)
    else
      let (s,rdy) = interp(init_done) in
      let (pc,acc,sp,(env, extra_args, trap_sp),(finished,led)) = s in
      let cy = counter() in
      let nbi = reg (fun c -> if  rdy then c + 1 else c) init 0 in
      if finished then (print_string "CY="; print_int cy; print_string "NBI="; print_int nbi);
      ((finished,rdy,init_done,led),acc)
  in
  let ((stop,rdy,_,o),acc) = reg step last ((false,false,false,false),val_unit) in
  (stop,not(rdy),o,acc)
;;

