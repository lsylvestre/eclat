open Ast
open Ast_subst

(* inline a program given in ANF, lambda-lifted form. The resulting program is
   in lambda-lifted-form but not necessarily in ANF-form. *)

(** [inline e] returns a non-ANF expression in which non-recursive functions
    are inlined *)
let rec inline e =
  match e with
  | E_letIn(p,e1,e2) ->
      (match p,e1 with
      | P_var x,E_fun _ -> inline @@ subst_e x e1 e2
      | _ -> E_letIn(p,inline e1,inline e2))
  | E_app(E_fun(p,e1),e2) ->
      (* substitution is needed (rather than a let-binding)
         since e2 could be a function (fun x -> e3)       (* no, first order now *) (* ah ? *) *)
      inline @@ E_letIn(p,e2,e1)
  | E_lastIn(x,e1,e2) ->
      let y = gensym () in
      E_lastIn(y,inline e1,subst_e x (E_var y) (inline e2))
  | e -> Ast_mapper.map inline e


let inl_pi pi =
  let main = inline pi.main in
  { pi with main }
