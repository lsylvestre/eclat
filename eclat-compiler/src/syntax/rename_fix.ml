open Ast
open Ast_subst

(* given a program, ensures that definitions of tail-recursive functions
   are of the form [let f = fix f (fun x -> e) in e1],
   i.e., with a same name [f] for both the direct and the tail call.
 *)

let rec rename f (e:e) : e =
  match e with
  | E_deco(e,d) -> E_deco(rename f e,d)
  | E_app(E_const(Op(TyConstr ty)),e) -> E_app(E_const(Op(TyConstr ty)),rename f e)
  | E_fix(g,(p,ty,e)) -> E_fix(f,(p,ty,subst_e g (E_var f) e))
  | e -> e

let rec rename_fix e =
  match e with
  | E_letIn(p,ty,e1,e2) ->
      (match un_annot e1,p with
      | E_fix _,P_var f -> E_letIn(P_var f,ty,rename f (rename_fix e1), rename_fix e2)
      | _ -> E_letIn(p,ty,rename_fix e1, rename_fix e2))
  | e -> Ast_mapper.map rename_fix e

let rename_pi pi =
  Map_pi.map rename_fix pi
