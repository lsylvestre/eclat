open Ast
open Ast_subst

(* remove simulation primitives like print and assert *)

let clean_exp ~no_print ~no_assert e =
  let rec clean e =
    match e with
    | E_letIn(p,E_fun(p2,e1),e2) ->
        E_letIn(p,E_fun(p2,clean e1),clean e2)
    | E_letIn((P_var f) as p,E_fix(g,(p2,e1)),e2) ->
        E_letIn(p,E_fix(g,(p2,subst_e g (E_var f) (clean e1))),clean e2) (* why in this file ? *)
    | E_letIn(p,e1,e2) ->
        E_letIn(p,clean e1,clean e2)
    | E_app(e1,e2) ->
        let opt = match un_annot e1 with
                  | E_const(Op(Runtime(Print | Print_string | Print_int | Print_newline))) ->
                       if no_print then Some (E_const(Unit)) else None
                  | E_const(Op(Runtime(Assert))) ->
                       if no_print then Some (E_const(Unit)) else None
                  | _ -> None in
        (match opt with
        | None -> E_app(clean e1,clean e2)
        | Some e0 -> e0)
    | e -> Ast_mapper.map clean e
  in clean e

let clean_pi ~no_print ~no_assert pi =
  Map_pi.map (clean_exp ~no_print ~no_assert) pi
