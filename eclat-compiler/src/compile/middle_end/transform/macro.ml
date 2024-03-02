open Ast
open Ast_subst

(* inline static top-level constants *)
let rec prop statics (e:e) : e =
  match statics with
  | [] -> e 
  | (x, Static_const(c))::sts ->
      prop sts (subst_e x (E_const c) e)
  | (x,Static_array _)::sts ->
      prop sts e
  | (x,Static_matrix _)::sts ->
      prop sts e


let inl_pi pi =
  (* assume that all toplevel ``static'' bindings have different names *)
  let main = prop pi.statics pi.main in
  { pi with main }
