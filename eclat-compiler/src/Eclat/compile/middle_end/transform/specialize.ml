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

let has_changed = ref false


let rec filter ty = 
  let open Types in
  match ty with
  | Ty_var{contents=Is ty} -> filter ty
  (*   | Ty_var({contents=Is ty1} as r) -> r := Is (filter ty1); ty *)
  | Ty_var _ -> ty
  | Ty_base _ -> ty
  | Ty_tuple ty_list ->
      Ty_tuple (List.map filter ty_list)
  | Ty_fun _ ->
      Ty_base TyB_unit
  | Ty_ref _ ->
      ty
  | Ty_array _ ->
      ty

let contains_fun ty =
  let open Types in
  let exception F in
  let rec aux = function
  | Ty_var{contents=Is ty} -> aux ty
  | Ty_var _ -> ()
  | Ty_base _ -> ()
  | Ty_tuple ty_list ->
      List.iter aux ty_list
  | Ty_fun _ ->
      raise F
  | Ty_ref _ ->
      ()
  | Ty_array _ ->
      ()
  in try aux ty; false with F -> true


(** in function [f = fun p -> ...],
    transforms the pattern [p] by replacing 
    each name that has a function type by constant () *)
let p_without_fun f p ty =
  let open Types in
  let rec remove_fun ty p = 
    match ty,p with
    | _,P_unit | Ty_base TyB_unit, _ -> P_unit
    | Ty_tuple ts,P_tuple ps -> 
        P_tuple (List.map2 remove_fun ts ps)
    | Ty_base (TyB_tuple tyBs),P_tuple ps -> 
        P_tuple (List.map2 remove_fun (List.map (fun tyB -> Ty_base tyB) tyBs) ps)
    | Ty_fun _,_ -> P_unit
    | Ty_var{contents=Is t},p ->
        remove_fun t p
    | _ -> p
  in
  remove_fun ty p

(** inlining high-order functions (must be called [!has_changed] stays true). *)
let specialize ds e = 
  let open Ast in
  let rec spec funcs e =
    match e with
    | E_app(E_const _,_) -> e
    | E_app(E_var f,xc2) ->
       
        (match SMap.find_opt f funcs with
        | None -> e
        | Some (ty_orig,E_tuple[E_var g;E_fix(_,(p,(ty,tyB),_))]) ->
           let p' = p_without_fun f p ty in
           let rec aux xc2 p =
             match xc2,p with
             | _,P_unit -> E_const Unit
             | E_var _, _ -> xc2
             | E_tuple es, P_tuple ps -> E_tuple (List.map2 aux es ps)
             | e,_ -> e
            in E_app(E_var f,aux xc2 p')
        (* | Some ((E_var _ | E_tuple _) as pat_e) ->  Ast_subst.subst_p_e (Pattern.exp2pat pat_e) E_app(E_var x,xc2) *)
        | Some (ty_orig,E_fun(p,(ty,tyB),e1)) -> 
           (* Format.fprintf Format.std_formatter "==> %a | %s %a %a \n" Types.pp_ty ty f Ast_pprint.pp_pat p Ast_pprint.pp_exp e1; *)
           (*Format.fprintf Format.std_formatter "==> %a %a \n" Types.pp_ty ty Ast_pprint.pp_exp xc2; *)
           (* 
           E_letIn(p,ty_orig,xc2, ty_annot ~ty:(Ty_base tyB) e1)*)
           let p' = p_without_fun f p ty in
           Inline.subst_ty ty_orig @@
           E_letIn(p,ty_orig,xc2,
           E_app(E_fun(p',(ty,tyB),e1),Pattern.pat2exp p'))
        | Some (ty_orig,E_fix(x,(p,(ty,tyB),e1))) ->
              let p' = p_without_fun f p ty in
              (* Format.fprintf Format.std_formatter "~~~~~~~> %a %a %a\n" Types.pp_ty ty Ast_pprint.pp_pat p' Ast_pprint.pp_pat p; *)
              (* Inline.subst_ty ty_orig @@*)
              (* E_letIn(p,ty_orig,xc2,
                let z = gensym ~prefix:x () in
                let e1' = (* Ast_subst.subst_p_e p (Pattern.pat2exp p')*) e1 in
                E_letIn(P_var z,Types.new_ty_unknown(), E_fix(x,(p',(ty,tyB),e1')), E_app(E_var z,Pattern.pat2exp p')))
*)
           Inline.subst_ty ty_orig @@
           E_letIn(p,ty_orig,xc2,
           E_app(E_fix(x,(p',(ty,tyB),e1)),Pattern.pat2exp p'))
        | Some _ ->
            assert false (* ill typed *)
      )
    | E_app(e1,xc) ->
        E_app(spec funcs e1,xc)
    | E_letIn(P_var x,ty,((E_var _ | E_tuple _) as xc1),e2) ->
        if contains_fun ty then
          (* copy propagation to remove aliasing of global functions *)
         (E_letIn(P_var x, ty, xc1, spec funcs e2))
          (* spec funcs (Ast_subst.subst_e x (ty_annot ~ty xc1) e2)*)
        else 
          E_letIn(P_var x,ty,xc1,spec funcs e2)
    | E_letIn(P_var x,ty0,(E_fix(x',(p,(ty,tyB),e3)) as e1),e2) ->
        let e3' = spec (SMap.add x'(ty,E_tuple[E_var x;e1]) funcs) e3 in
        if contains_fun ty then (has_changed := true; spec (SMap.add x (ty,E_fix(x',(p,(filter ty,tyB),e3'))) funcs) e2) 
        else
        E_letIn(P_var x,ty0,(E_fix(x',(p,(ty,tyB),e3'))),spec funcs e2)
    | E_letIn(P_var x,ty0,((E_fun(p,(ty,tyB),e3))),e2) ->
        let e3' = spec funcs e3 in
        if contains_fun ty then (
           has_changed := true; 
           spec (SMap.add x (ty,E_fun(p,(filter ty,tyB),e3')) funcs) e2)
        else
        E_letIn(P_var x,ty0,(E_fun(p,(ty,tyB),e3')),spec funcs e2)
    | E_letIn(p,ty,e1,e2) ->
        E_letIn(p,ty,spec funcs e1,spec funcs e2)
    | e -> Ast_mapper.map (spec funcs) e
  in
  spec SMap.empty e



let rec list_update (x,v) = function
| [] -> []
| (y,v')::l -> if x = y then (x,v)::l else (y,v')::list_update (x,v) l

let normalize pi =
  let pi = Ast_rename.rename_pi pi in (* seems needed *)
  Propagation.propagation_pi @@ Let_floating.float_pi pi ;;

let oo = ref 0 ;;

let rec specialize_pi pi =
  has_changed := false;
  let pi_norm = normalize pi in
  let main = specialize [] pi_norm.main in
  let pi_res = { pi_norm with main } in 
  (* Format.fprintf Format.std_formatter "==> %a \n" Ast_pprint.pp_exp pi_res.main; *)
  flush stdout;
  if !oo > 10 then   assert false else incr oo; 
  if !has_changed then specialize_pi pi_res else normalize pi_res

