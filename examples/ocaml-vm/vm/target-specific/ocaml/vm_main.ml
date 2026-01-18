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
