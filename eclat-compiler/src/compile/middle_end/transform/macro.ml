open Ast
open Ast_subst

(* inline a program given in ANF, lambda-lifted form. The resulting program is
   in lambda-lifted-form but not necessarily in ANF-form. *)

(** [inline e] returns a non-ANF expression in which non-recursive functions
    are inlined *)
let rec prop statics (e:e) : e =
  match statics with
  | [] -> e 
  | (x, Static_const(c))::sts ->
      prop sts (subst_e x (E_const c) e)
  | (x,Static_array _)::sts ->
      prop sts e


let inl_pi pi =
  (* assume that all toplevel ``static'' bindings have different names *)
  let main = prop pi.statics pi.main in
  { pi with main }
