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

(* list function names free in [e] that are applied in [e] *)

let applyed e =
  let r = ref SMap.empty in
  let rec applyed_e = function
  | E_app(E_var x,e1) ->
      r := SMap.add x () !r;
      applyed_e e1
  | e -> Ast_mapper.iter applyed_e e
  in
  applyed_e e;
  let vs = Free_vars.fv e in
  SMap.filter (fun x _ -> SMap.mem x vs) !r
  
(** returns [true] if one of the names[xs] is called in [e] *)
let applyed_xs xs e =
  let vs = Free_vars.fv e in
  let xs_free = SMap.filter (fun x _ -> SMap.mem x vs) xs in
  let exception Found in
  let rec find e = 
    match e with
    | E_app(E_var x,e1) ->
       if SMap.mem x xs_free 
       then raise Found 
       else ()
    | e -> Ast_mapper.iter find e
  in 
  try (find e; false) 
  with Found -> true
  

(** return true if [x] is called in the body of function [v] *)
let hof x v =
  match v with
  | E_fun(p,e) ->
      let xs = vars_of_p p in
      applyed_xs xs e
  | E_fix(g,(p,e)) ->
      let xs = vars_of_p p in
      assert (not (SMap.mem g xs));
      applyed_xs xs e
  | _ -> false

(** in function [f = fun p -> ...],
    transforms the pattern [p] by replacing 
    each name that has a function type by constant () *)
let p_without_fun f p =
  let t =
    try Hashtbl.find Typing.signatures f (* need to types the program before *) 
    with
    | Not_found ->
       Types.unknown() (*TODO : check *) 
  in
  let open Types in
  let targ = match canon t with 
             | T_fun{arg} -> arg
             | _ -> Types.unknown() (* assert false ?*) 
  in
  let rec remove_fun t p = 
    match canon t,p with 
    | _,P_unit -> P_unit
    | T_tuple ts,P_tuple ps -> 
        P_tuple (List.map2 remove_fun ts ps)
    | T_fun{arg},_ -> P_unit
    | T_var{contents=Ty t},p ->
        remove_fun t p
    | _ -> p
  in
  remove_fun targ p

(** inlining high-order functions (must be called [!has_changed] stays true). *)
let specialize ds e =
  let open Ast in
  let rec spec funcs e =
    match e with
    | E_app(E_const _,_) -> e
    | E_app(E_var f,xc2) ->
        (match SMap.find_opt f funcs with
        | None -> e
        | Some (E_tuple[E_var g;E_fix(_,(p,_))]) ->
           let p' = p_without_fun f p in
           let rec aux xc2 p =
             match xc2,p with
             | _,P_unit -> E_const Unit
             | E_var _, _ -> xc2
             | E_tuple es, P_tuple ps -> E_tuple (List.map2 aux es ps)
             | e,_ -> e
            in E_app(E_var f,aux xc2 p')
        (* | Some ((E_var _ | E_tuple _) as pat_e) ->  Ast_subst.subst_p_e (exp2pat pat_e) E_app(E_var x,xc2)*)
        | Some (E_fun(p,e1)) -> E_letIn(p,xc2,e1)
        | Some (E_fix(x,(p,e1))) ->
              let p' = p_without_fun f p in
              E_letIn(p,xc2,
                let z = gensym ~prefix:x () in
                let e1' = (* Ast_subst.subst_p_e p (Pattern.pat2exp p')*) e1 in
                E_letIn(P_var z, E_fix(x,(p',e1')), E_app(E_var z,Pattern.pat2exp p')))

        | Some _ ->
            assert false (* ill typed *)
      )
    | E_app(e1,xc) ->
        E_app(spec funcs e1,xc)
    | E_letIn(P_var x,((E_var _ | E_tuple _) as xc1),e2) ->
        (* copy propagation to remove aliasing of global functions *)
        spec funcs (Ast_subst.subst_e x xc1 e2)
    | E_letIn(P_var x,(E_fix(x',(p,_)) as e1),e2) ->
        let e1' = spec (SMap.add x' (E_tuple[E_var x;e1]) funcs) e1 in
        if hof x e1 then (has_changed := true; spec (SMap.add x e1' funcs) e2) 
        else
        E_letIn(P_var x,e1',spec funcs e2)
    | E_letIn(P_var x,((E_fun _) as e1),e2) ->
        let e1' = spec funcs e1 in
        if hof x e1 then (has_changed := true; spec (SMap.add x e1' funcs) e2)
        else
        E_letIn(P_var x,e1',spec funcs e2)
    | E_letIn(p,e1,e2) ->
        E_letIn(p,spec funcs e1,spec funcs e2)
    | e -> Ast_mapper.map (spec funcs) e
  in
  spec SMap.empty e



let rec list_update (x,v) = function
| [] -> []
| (y,v')::l -> if x = y then (x,v)::l else (y,v')::list_update (x,v) l

let normalize pi =
  let pi = Ast_rename.rename_pi pi in (* seems needed *)
  Propagation.propagation_pi @@ Let_floating.float_pi pi ;;

let rec specialize_pi pi =
  has_changed := false;
  let pi_norm = normalize pi in
  let _ = Typing.typing_with_argument ~collect_sig:true pi [] in
  let main = specialize [] pi_norm.main in
  let pi_res = { pi_norm with main } in 
  if !has_changed then specialize_pi pi_res else normalize pi_res

