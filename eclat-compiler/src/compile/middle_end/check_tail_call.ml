open Ast

let check e =
  let exception Found of x * Prelude.loc in
  let rec find ~tailcall xs e =
    match e with
    | E_deco(e,_) -> find ~tailcall xs e
    | E_fun(_,_,e) ->
        find ~tailcall xs e
    | E_fix(f,(_,_,e)) ->
        find ~tailcall (SMap.add f (loc_of e) xs) e
    | E_letIn(p,_,e1,e2) ->
        find ~tailcall:(evaluated e1) xs e1;
        find ~tailcall xs e2
    | E_if(e1,e2,e3) ->
        find ~tailcall:false xs e1;
        find ~tailcall xs e2;
        find ~tailcall xs e3
    | E_case(e1,hs,e_els) ->
        find ~tailcall:false xs e1;
        List.iter (fun (_,ei) ->
          find ~tailcall xs ei) hs;
        find ~tailcall xs e_els
    | E_match(e1,hs,eo) ->
        find ~tailcall:false xs e1; 
        List.iter (fun (_,(_,ei)) -> find ~tailcall xs ei) hs;
        Option.iter (find ~tailcall xs) eo
    | E_app(e1,e2) ->
       (match Ast_undecorated.remove_deco e1 with
       | E_var f ->
           (if not(tailcall) && SMap.mem f xs 
            then raise (Found(f,SMap.find f xs)) else ());
           find ~tailcall:false xs e1
       | E_const(Op(TyConstr _)) ->
           find ~tailcall xs e2
       | _ ->
           find ~tailcall:false xs e1;
           find ~tailcall:false xs e2)
    | e -> Ast_mapper.iter (find ~tailcall:false xs) e
  in 
  try (find ~tailcall:true SMap.empty e; None) 
  with Found (x,loc) -> Some (x,loc)

let check_pi pi =
  match check pi.main with
  | None -> ()
  | Some (x,loc) ->
      (let open Prelude.Errors in
       error ~loc
          (fun fmt -> 
             Format.fprintf fmt "Function %s is not tail recursive" x))