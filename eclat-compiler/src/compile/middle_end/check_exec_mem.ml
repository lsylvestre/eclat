open Ast

let lvl = ref 0
let h = Hashtbl.create 10 ;;

let check e =
  Hashtbl.clear h;
  let exception Found in
  let rec loop e =
    match e with
    | E_exec(e,_,_,_) ->
        incr lvl;
        loop e;
    | E_array_get(x,e1) ->
         if Hashtbl.mem h x && Hashtbl.find h x <> !lvl then raise Found;
        Hashtbl.add h x !lvl;
        loop e1
    | E_array_set(x,e1,e2) ->
        if Hashtbl.mem h x && Hashtbl.find h x <> !lvl then raise Found;
        Hashtbl.add h x !lvl;
        loop e1;
        loop e2
    | e -> Ast_mapper.iter loop e
  in 
  try (loop e; true) 
  with Found -> false

let check_pi pi =
  if check pi.main then () else
      (let open Prelude.Errors in
       warning
          (fun fmt -> 
             Format.fprintf fmt "Access to a same array in different exec"))