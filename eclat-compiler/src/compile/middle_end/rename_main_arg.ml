open Ast
open Ast_subst

let rec rename_main_arg_exp e =
  match e with
  | E_fun(P_var x,sig_ty,e1) ->
      subst_e x (E_var "argument") e1
  | E_letIn(p,ty,e1,e2) -> E_letIn(p,ty,e1,rename_main_arg_exp e2)
  | E_if(e1,e2,e3) -> E_if(e1, rename_main_arg_exp e2, rename_main_arg_exp e3)
  | _ -> 
    Format.(fprintf std_formatter "==========>%a\n" Ast_pprint.pp_exp (e));
    assert false (* can occur ? *)

let rec rename_main_arg_pi pi =
  { pi with main = rename_main_arg_exp pi.main }