open Ast

module Deadcode_elimination = struct

  (* not an optimisation: needed for moving declaration correctly *)

  (* returns [true] if name [var] is free in [e] *)
  let in_fv ?(xs=SMap.empty) var e =
    let open Ast in
    let exception Found in
    let rec aux xs = function
    | E_var x ->
        if SMap.mem x xs then () else
        if x = var then raise Found
    | E_match(e1,hs,eo) ->
        aux xs e1;
        List.iter (fun (_,(p,ei)) ->
          let ys = vars_of_p p in
          let xs' = xs++ys in
          (aux xs' ei)) hs;
        Option.iter (aux xs) eo
    | E_letIn(_,_,E_trap _,_) -> ()
    | E_letIn(p,_,e1,e2) ->
        let ys = vars_of_p p in
        let xs' = xs ++ ys in
        aux xs e1; aux xs' e2
    | E_fun(p,_,e1) ->
        let ys = vars_of_p p in
        let xs' = xs++ys in
        aux xs' e1
    | E_reg((p,_,e1),e0,_) ->
        let ys = vars_of_p p in
        let xs' = xs++ys in
        aux xs' e1;
        aux xs e0
    | E_fix(f,(p,_,e)) ->
        let ys = vars_of_p p in
        let xs' = SMap.add f () @@ (xs++ys) in
        aux xs' e
    | E_exec(e1,_,eo,_) ->
        (* note: no recursive call on [e2] *)
        Option.iter (aux xs) eo
    | E_par _ ->
        (* note: no recursive call *)
        ()
    | E_loop _
    | E_suspend _ -> ()
    | e -> Ast_mapper.iter (aux xs) e
    in
    try aux xs e ; false with Found -> true


  let rec deadcode_elim = function
    | E_letIn(P_var f,tyf,(E_fix(h,(p,(ty,tyB),e1)) as phi),e2) ->
        if (f <> h) then deadcode_elim (E_letIn(P_var f,(*tyf*)Types.new_ty_unknown(),
                                           (E_fix(f,(p,(Types.new_ty_unknown(),Types.new_tyB_unknown())(*(ty,tyB)*), 
                                               Ast_subst.subst_e h (E_var f) e1))),e2)) else
        if in_fv f e2 then E_letIn(P_var f,(*tyf*)Types.new_ty_unknown(),deadcode_elim phi, deadcode_elim e2) else deadcode_elim e2
    | e -> Ast_mapper.map deadcode_elim e

  let deadcode_elimination_pi pi =
    let pi = Ast_rename.rename_pi pi in
    (* currently, renaming seems necessary 
       for proper deadcode elimination *)
    {pi with main = deadcode_elim pi.main }

end


(* a list is needed to maintain an order in the environment *)

let rec map_under_exec_and_par e =
  let decl env e =
    List.fold_left (fun e (x,v) -> 
        if not(SMap.mem x@@Free_vars.fv e) (* todo: avoid the traversal of [e] for each name [x] *)
        then e 
        else E_letIn(P_var x, Types.new_ty_unknown(), v,e)
    ) e env
  in
  let rec aux env e =
    match e with
    | E_letIn(P_var f,ty,(E_fix(g,(p,(ty1,tyB2),e1)) as phi),e2) ->
        let e1' = aux env e1 in (* assume [env] disjoint of [g] and [p] *)
        let phi' = E_fix(g,(p,(ty1,tyB2),(* aux env*) e1')) in   (* was [aux env e1'] before: a bug ? *)
        E_letIn(P_var f,Types.new_ty_unknown(),phi',aux ((f,phi')::env) e2)
    | E_par(es) ->
        E_par(List.map (fun e -> map_under_exec_and_par (decl env e)) es)
    | E_exec(e1,e0,eo,l) ->
        E_exec(map_under_exec_and_par (decl env e1), aux env e0, Option.map (aux env) eo, l) 
        (* why not ((decl env (aux env e1))) ? *)
    | E_loop(e1) ->
        E_loop(map_under_exec_and_par (decl env e1))
    | E_letIn(P_var x,tyB,E_trap tyB', e1) ->
        E_letIn(P_var x,tyB,E_trap tyB', map_under_exec_and_par (decl env e1))
    | E_suspend(e1,x) ->
        E_suspend(map_under_exec_and_par (decl env e1),x)
    | e -> 
       (** no special case for [reg] 
           because this never occurs under an exec **)
       Ast_mapper.map (aux env) e
  in aux [] e


let move_down_gfun_under_exec_and_par_pi pi =
  let main = map_under_exec_and_par pi.main in
  {pi with main}
  |> Deadcode_elimination.deadcode_elimination_pi
    (** deadcode elimination is absolutely needed
        because [map_under_exec_and_par] introduces a lot of
        unused function declarations **)
