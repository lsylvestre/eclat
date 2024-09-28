open Ast
open Ast_subst

let rec name e =
  match e with
  | E_fun(p,ty,e1) -> (* anonymus functions are named *)
      let f = gensym () in
      E_letIn(P_var f, Types.new_ty_unknown(), E_fun(p,ty,name e1), E_var f)
  | E_fix(f,(p,ty,e1)) -> (* anonymus tail-recursive functions are named *)
      E_letIn(P_var f, Types.new_ty_unknown(), E_fix(f,(p,ty,name e1)), E_var f)
  | E_letIn(p,ty,E_fun(p2,ty2,e1),e2) ->
      E_letIn(p,ty,E_fun(p2,ty2,name e1),name e2)
  | E_letIn(p,ty,E_fix(x,(p2,ty2,e1)),e2) ->
      E_letIn(p,ty,E_fix(x,(p2,ty2,name e1)),name e2)
  | e -> Ast_mapper.map name e


let name_top (e:e) : e =
  match e with
  | E_fun(p,ty,e) -> E_fun(p,ty,name e)
  | E_fix(f,(p,ty,e)) -> E_fix(f,(p,ty,name e))
  | _ -> e

let name_pi pi =
  Map_pi.map name_top pi
