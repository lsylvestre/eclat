(**

 ============================================================
 "specialiation", i.e., elimination of higher-order functions
 ============================================================

  we have to be able to specialize expressions like :

     [let rec foo (g,n) = 
        let x = 
          let y = 2*n in 
          g  
        in 
        let z = x(1,2) in 
        foo (g,1)
      in 
      foo ((+),1)]

  To detect that [foo] has one functional parameter [g] 
  and then specialize [foo], we first move up bindings (by let floating):

     [let rec foo (g,n) = 
        let y = 2*n in 
        let x = g in 
        let z = x(n,2) in 
        foo (g,n+1) 
      in 
      foo ((+),1)]

  Then, we propagate constants and copies:

     [let rec foo (g,n) = 
        let y = 2*n in 
        let z = g(n,2) in 
        foo (g,n+1) 
      in 
      foo ((+),1)]

  Then, we detect that the parameter [g] is applied in the definition of [foo]

  And, assuming that recursive function-calls do not change 
  the functional parameters (e.g., [let rec foo g = foo h] is forbidden)
  we remove this parameter like this :

     [let g = (+) in
      let n = 1 in
      let rec foo ((),n) = 
        let y = 2*n in 
        let z = g(n,2) in 
        foo ((),n+1) 
      in 
      foo ((),n)]

    The formal parameters and arguments (when called) of foo are remplaced by ()
    by traversing structurally the type signature of foo.

*)

open Ast
open Types

let has_changed = ref false

let not_tyB ty =
  match canon_ty ty with
  | Ty_base _ -> false
  | _ -> true


let rec filter ty = 
  let open Types in
  match ty with
  | Ty_var{contents=Is ty} -> filter ty
  (*   | Ty_var({contents=Is ty1} as r) -> r := Is (filter ty1); ty *)
  | Ty_base _ -> ty
  | Ty_tuple ty_list ->
      Ty_tuple (List.map filter ty_list)
  | Ty_fun _
  | Ty_ref _
  | Ty_array _
  | Ty_signal _ 
  | Ty_trap _
  | Ty_size _
  -> Ty_base TyB_unit
| Ty_var _ -> ty


(** in function [f = fun p -> ...],
    transforms the pattern [p] by replacing 
    each name that has a function type by constant () *)
let p_without_non_basic_values f p ty =
  let open Types in
  let rec aux ty p = 
    match ty,p with
    | _,P_unit | Ty_base TyB_unit, _ -> P_unit
    | Ty_tuple ts,P_tuple ps -> 
        P_tuple (List.map2 aux ts ps)
    | (Ty_fun _ | Ty_array _ | Ty_ref _ | Ty_trap _ | Ty_signal _),_ -> P_unit
    | Ty_var{contents=Is t},p ->
        aux t p
    (* | Ty_var _,_ -> 
        P_unit (* this case is important to remove function parameters 
                  that does not occur in the function body, e.g.,
                  let f(g) = 42 in f(fun x -> x) *)
    *)| _ -> p
  in
  aux ty p



(* ok but very slow *)
let specialize_slow e = 
  let rec spec e =
    match e with
    | E_letIn(P_var x,ty,e1,e2) ->
        (match e1 with
        | E_fun(_,(ty,_),_) when not_tyB ty ->  
              has_changed := true;
              Ast_subst.subst_e x ~when_var:Inline.subst_ty e1 e2
        | E_fix(f,(p,(ty,tyB),e0)) when not_tyB ty ->            
              let p' = p_without_non_basic_values f p ty in
              (* has_changed := true;*)
              let e0' = Ast_subst.subst_e f ~when_var:Inline.subst_ty 
                 (E_fun(p,(ty,tyB),E_app(E_var f,Pattern.pat2exp p'))) e0 in
              let e1' = E_fun(p,(ty,tyB),
                              E_app(E_fix(f,(p',(filter ty,tyB), e0')),Pattern.pat2exp p')) in
              Ast_subst.subst_e x ~when_var:Inline.subst_ty e1' e2
        | _ -> E_letIn(P_var x,ty,spec e1, spec e2))
    | e -> Ast_mapper.map spec e
  in
  spec e



let specialize e = 
  let rec spec env e =
    match e with
    | E_var x -> (match SMap.find_opt x env with
                  | None -> e
                  | Some e0 -> Inline.subst_ty e0)
    | E_letIn(P_var x,ty,e1,e2) ->
        (match e1 with
        | E_fun(_,(ty,_),_) when not_tyB ty ->
              has_changed := true;
              spec (SMap.add x (spec env e1) env) e2
        | E_fix(f,(p,(ty,tyB),e0)) when not_tyB ty ->            
              has_changed := true;
              let p' = p_without_non_basic_values f p ty in
              (* has_changed := true;*)
              let e0' = spec (SMap.add f
                 (E_fun(p,(ty,tyB),E_app(E_var f,Pattern.pat2exp p'))) env) e0 in
              let e1' = E_fun(p,(ty,tyB),
                              E_app(E_fix(f,(p',(filter ty,tyB), e0')),Pattern.pat2exp p')) in
              spec (SMap.add x e1' env) e2
        | _ -> E_letIn(P_var x,ty,spec env e1, spec env e2))
    | e -> Ast_mapper.map (spec env) e
  in
  spec SMap.empty e


let normalize pi =
  let pi = Ast_rename.rename_pi pi in (* seems needed *)
  let pi = Let_floating.float_pi pi in
  let pi = Propagation.propagation_pi pi in 
  let pi = Rename_fix.rename_pi pi in
  pi ;;


let rec specialize_pi pi =
  has_changed := false;
  
  let pi_norm = normalize pi in
  flush stdout;
  let main = specialize pi_norm.main in
  let pi = {pi with main} in
  let _ = Typing.typing_with_argument pi [] in 
  if !has_changed then specialize_pi pi else normalize pi
