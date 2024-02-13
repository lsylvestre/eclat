
open Ast


module Deadcode_elimination = struct

  (* not an optimisation: needed for moving declaration correctly *)

  let in_fv ?(xs=SMap.empty) var e =
    let open Ast in
    let exception Found in
    let rec aux xs = function
    | E_var x ->
        if x = var then raise Found
    | E_match(e1,hs,eo) ->
        aux xs e1;
        List.iter (fun (_,(p,ei)) ->
          let ys = vars_of_p p in
          let xs' = xs++ys in
          (aux xs' ei)) hs;
        Option.iter (aux xs) eo
    | E_letIn(p,e1,e2) ->
        let ys = vars_of_p p in
        let xs' = xs ++ ys in
        aux xs e1; aux xs' e2
    | E_fun(p,e1) ->
        let ys = vars_of_p p in
        let xs' = xs++ys in
        aux xs' e1
    | E_reg((p,e1),e0,_) ->
        let ys = vars_of_p p in
        let xs' = xs++ys in
        aux xs' e1;
        aux xs e0
    | E_fix(f,(p,e)) ->
        let ys = vars_of_p p in
        let xs' = SMap.add f () @@ (xs++ys) in
        aux xs' e
    | E_exec(e1,e2,_) ->
        (* note: no recursive call on [e1] *)
        aux xs e2
    | E_lastIn(x,e1,e2) ->
        let xs' = SMap.add x () xs in
        aux xs e1; aux xs' e2
    | E_par _ ->
        (* note: no recursive call *)
        ()
    | e -> Ast_mapper.iter (aux xs) e
    in
    try aux xs e ; false with Found -> true


  let rec deadcode_elim = function
    | E_letIn(P_var f,(E_fix(h,(p,e1)) as phi),e2) ->
        if (f <> h) then deadcode_elim (E_letIn(P_var f,(E_fix(f,(p,Ast_subst.subst_e h (E_var f) e1))),e2)) else
        if in_fv f e2 then E_letIn(P_var f,deadcode_elim phi, deadcode_elim e2) else deadcode_elim e2
    | e -> Ast_mapper.map deadcode_elim e

  let deadcode_elimination_pi pi =
    {pi with main = deadcode_elim pi.main }

end


(* a list is needed to maintain an order in the environment *)

let rec map_under_exec_and_par e =
  let decl env e =
    List.fold_right (fun (x,v) e -> E_letIn(P_var x,v,e)) (List.rev env) e
  in
  let rec aux env e =
    match e with
    | E_letIn(P_var f,(E_fix(g,(p,e1))),e2) ->
        let e1' = aux env e1 in (* assume [env] disjoint of [g] and [p] *)
        let phi' = E_fix(g,(p,aux env e1')) in
        E_letIn(P_var f,phi',aux ((f,phi')::env) e2)
    | E_par(e1,e2) ->
        E_par(map_under_exec_and_par (decl env e1), map_under_exec_and_par (decl env e2))
    | E_exec(e1,e0,l) ->
        E_exec(map_under_exec_and_par (decl env e1), aux env e0,l)
    | e -> Ast_mapper.map (aux env) e
  in aux [] e


let move_down_gfun_under_exec_and_par_pi pi =
  let main = map_under_exec_and_par pi.main in
  {pi with main}
  |> Deadcode_elimination.deadcode_elimination_pi
    (* deadcode elimination is needed
       because [map_under_exec_and_par] introduces a lot of
       unused function declarations *)
