open Ast
open Pattern

(* value propagation, including inlining and constant/copy propagation *)

let rec inline env e =
  match e with
  | E_fun(p,e) ->
      let xs = vars_of_p p in
      E_fun(p,inline (SMap.filter (fun x _ -> not (SMap.mem x xs)) env) e)
  | E_fix(f,(p,e)) ->
      let xs = vars_of_p p in
      E_fix(f,(p,inline (SMap.filter (fun x _ -> not (SMap.mem x xs)) env) e))
  | E_var x ->
      (match SMap.find_opt x env with
       | None -> e
       | Some v -> v)
  | E_letIn(p,e1,e2) ->
      let e1 = inline env e1 in
      let r = bindings p e1 in
      let r1, r2 = SMap.partition (fun _ e -> evaluated e || is_variable e) r in
      let env' = env++r1 in
      let xs,es = List.split @@ SMap.bindings r2 in
      if xs = [] then inline env' e2 else
      let ps = List.map (fun x -> P_var x) xs in
      List.fold_right2 (fun p e acc -> E_letIn(p,e,acc)) ps es (inline env' e2)
      (* E_letIn(m,group_ps ps,group_es es,inline env' e2) *)
  | e -> Ast_mapper.map (inline env) e

let normalize e =
  (* Ast_rename.rename_e @@ *)
  Instantiate.instantiate (inline Ast.SMap.empty (Ast_rename.rename_e ~statics:[] e))
