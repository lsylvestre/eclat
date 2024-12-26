open Ast

let h = Hashtbl.create 10 ;;

let check e =
  Hashtbl.clear h;
  let exception Found in
  let rec loop e =
    match e with
    | E_exec(e,_,eo,_) ->
       (match eo with
        | None | Some (E_const (Bool false)) -> ()
        | Some _ -> 
           Hashtbl.iter (fun x _ -> 
              Hashtbl.replace h x true) h;
           loop e)
    | E_letIn(P_var x, _, E_array_create _,e) ->
        Hashtbl.add h x false;
        loop e
    | E_array_get(x,e1) ->
        if Hashtbl.mem h x && Hashtbl.find h x then raise Found;
        loop e1
    | E_array_set(x,e1,e2) ->
        if Hashtbl.mem h x && Hashtbl.find h x then raise Found;
        loop e1;
        loop e2
    | e -> Ast_mapper.iter loop e
  in 
  try (loop e; true) 
  with Found -> false

let check_pi pi =
  if check pi.main then () else
      (let open Prelude.Errors in
       error
          (fun fmt -> 
             Format.fprintf fmt "Access to an array defined outside the current resetable exec block"))

