open Ast
open Ast_subst

let has_changed = ref false

let applyed e =
  let open Ast in

  let rec applied_list xs es =
    List.fold_left (fun acc ei -> acc ++ aux xs ei) SMap.empty es
  and aux xs = function
  | E_deco(e,_) ->
      aux xs e
  | E_var x ->
      SMap.empty
  | E_const _ ->
      SMap.empty
  | E_if(e1,e2,e3) ->
      aux xs e1 ++ aux xs e2 ++ aux xs e3
  | E_case(e1,hs,e_els) ->
      aux xs e1 ++
      List.fold_left (fun acc (c,ei) -> (acc ++ aux xs ei)) SMap.empty hs ++
      aux xs e_els
  | E_match(e1,hs,eo) ->
      let s = aux xs e1 ++
              List.fold_left (fun acc (inj,(p,ei)) ->
                let ys = vars_of_p p in
                let xs' = xs ++ ys in
                (acc ++ aux xs' ei)) SMap.empty hs in
      (match eo with
       | None -> s
       | Some e -> s ++ aux xs e)
  | E_letIn(p,e1,e2) ->
      let ys = vars_of_p p in
      let xs' = xs ++ ys in
      aux xs e1 ++ aux xs' e2
  | E_app(E_var f,e2) ->
      (if SMap.mem f xs then SMap.empty else SMap.singleton f ())
      ++ aux xs e2
  | E_app(e1,e2) ->
      aux xs e1 ++ aux xs e2
  | E_fun(p,e1) ->
      let ys = vars_of_p p in
      let xs' = xs++ys in
      aux xs' e1
  | E_fix(f,(p,e)) ->
      let ys = vars_of_p p in
      let xs' = SMap.add f () @@ (xs++ys) in
      aux xs' e
  | E_tuple(es) ->
      applied_list xs es
  | E_reg((p,e1), e0,_) ->
      let ys = vars_of_p p in
      let xs' = xs ++ ys in
      aux xs' e1 ++ aux xs e0
  | E_exec(e1,e2,_) ->
      aux xs e1 ++ aux xs e2
  | E_ref(e1) ->
      aux xs e1
  | E_get(e1) ->
      aux xs e1
  | E_set(e1,e2) ->
      aux xs e1 ++ aux xs e2
  | E_local_static_array(e1,e2,_) ->
      aux xs e1 ++ aux xs e2
  | E_array_length _ ->
      SMap.empty
  | E_array_get(_,e1) ->
      aux xs e1
  | E_array_set(_,e1,e2) ->
      aux xs e1 ++ aux xs e2
  | E_local_static_matrix(e1,es,_) ->
      aux xs e1 ++ applied_list xs es
  | E_matrix_size _ ->
      SMap.empty
  | E_matrix_get(_,es) ->
      applied_list xs es
  | E_matrix_set(_,es,e2) ->
      applied_list xs es ++ aux xs e2
  | E_par(es) ->
      applied_list xs es 
  | E_for(i,e_st1,e_st2,e3,_) ->
      aux xs e_st1 ++ aux xs e_st2 ++
      (let xs' = SMap.add i () xs in
       aux xs' e3)
  | E_generate((p,e1),e2,e_st3,_) ->
      let ys = vars_of_p p in
      (let xs' = xs ++ ys in
       aux xs' e1) ++ aux xs e2 ++ aux xs e_st3
  in
  aux SMap.empty e


let hof x e =
  match e with
  | E_fun(p,e) ->
      let xs = applyed e in
      vars_of_p p |> SMap.filter (fun x _ -> SMap.mem x xs) |> SMap.is_empty |> not

  | E_fix(g,(p,e)) ->
      let xs = applyed e in
      vars_of_p p |> SMap.filter (fun y _ -> y <> g && y <> x && SMap.mem y xs) |> SMap.is_empty |> not
  | _ -> false

let p_without_fun f p =
  let t = try Hashtbl.find Typing.signatures f with Not_found -> Types.unknown() (*TODO : check *) in
                     let open Types in
                     let targ = match canon t with T_fun{arg} -> arg | _ -> Types.unknown() (* assert false*) in
                     let rec remove_fun t p = match canon t,p with 
                      | _,P_unit -> P_unit
                      | T_tuple ts,P_tuple ps -> P_tuple (List.map2 remove_fun ts ps)
                      | T_fun{arg},_ -> P_unit
                      | T_var{contents=Ty t},p -> remove_fun t p
                      | _ -> p in
  remove_fun targ p



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
        | Some (E_fun(p,e1)) -> Anf.anf @@ E_letIn(p,xc2,e1)
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
   (*  | E_letIn(P_var x,(E_fix(f,(p,e1))),e2) ->
        let p' = 
                let t = try Hashtbl.find Typing.signatures f with Not_found -> Types.unknown() (*TODO : check *) in
                     let open Types in
                     let targ = match canon t with T_fun{arg} -> arg | _ -> Types.unknown() (* assert false*) in
                     let rec keep_fun t p = match canon t,p with 
                      | _,P_unit -> P_unit
                      | T_tuple ts,P_tuple ps -> P_tuple (List.map2 keep_fun ts ps)
                      | T_fun{arg},P_var x -> P_var x
                      | T_var{contents=Ty t},p -> keep_fun t p
                      | _ -> P_unit in
                      keep_fun targ p
              in

        let e1' = spec (SMap.add x (Pattern.pat2exp p') funcs) e1 in
        if hof x e1 then spec (SMap.add x e1' funcs) e2 else
        E_letIn(P_var x,e1',spec funcs e2)*)
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


let specialize_pi pi =
  has_changed := false;
  let _ = Typing.typing_with_argument ~collect_sig:true pi [] in
  (* let pi = Anf.anf_pi pi in*) (* already in anf-form *)
  let main = specialize [] pi.main in
  { pi with main }
  |> Propagation.propagation_pi    (* needed *)
