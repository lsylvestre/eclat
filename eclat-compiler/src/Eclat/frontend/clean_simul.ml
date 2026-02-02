open Ast
open Ast_subst

(* remove simulation primitives like print and assert *)

let clean_exp ~no_print ~no_assert ~genv e =
  let rec clean e =
    match e with
    | E_letIn(p,ty,E_fun(p2,sigty,e1),e2) ->
        E_letIn(p,ty,E_fun(p2,sigty,clean e1),clean e2)
    | E_letIn((P_var f) as p,ty,E_fix(g,(p2,sigty,e1)),e2) ->
        E_letIn(p,ty,E_fix(g,(p2,sigty,subst_e ~warning:false g (E_var f) (clean e1))),clean e2) (* why in this file ? *)
    | E_letIn(p,ty,e1,e2) ->
        E_letIn(p,ty,clean e1,clean e2)
    | E_app(e1,e2) ->
        let opt = match un_annot e1 with
                  | E_const(Op(Runtime(External_fun(x,_)))) ->
                      if no_print then
                          (match SMap.find_opt x genv.operators with
                           | Some (t,(_,_,is_imp,_)) ->
                              if is_imp 
                              then (Some (E_letIn(P_var x,Types.new_ty_unknown(),e2,E_const(Unit))))
                              else None
                           | None -> None)
                      else None
                   | _ -> None in
        (match opt with
        | None -> E_app(clean e1,clean e2)
        | Some e0 -> e0)
    | E_assert(e1,loc) ->
        let x = gensym ~prefix:"tmp" () in
        if no_assert then E_letIn(P_var x,Types.new_ty_unknown(),e1,E_const(Unit))
        else E_assert(clean e1,loc)
    | e -> Ast_mapper.map clean e
  in clean e

let clean_pi ~no_print ~no_assert pi =
  if no_print || no_assert then
    Map_pi.map (clean_exp ~no_print ~no_assert ~genv:pi.genv) pi
  else pi
