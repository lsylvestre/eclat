
open Ast

let fv_var xs x =
  if SMap.mem x xs then SMap.empty else SMap.singleton x ()

let fv ?(get_sig=true) ?(get_arrays=true) ?(xs=SMap.empty) e =
  let open Ast in
  let rec fv_list xs es =
    List.fold_left (fun acc ei -> acc ++ aux xs ei) SMap.empty es
  and aux xs = function
  | E_deco(e,_) ->
      aux xs e
  | E_var x ->
      fv_var xs x
  | E_const _ ->
      SMap.empty
  | E_if(e1,e2,e3) ->
      aux xs e1 ++ aux xs e2 ++ aux xs e3
  | E_case(e1,hs,e_els) ->
      aux xs e1 ++
      fv_list xs (List.map snd hs) ++
      aux xs e_els
  | E_match(e1,hs,eo) ->
      let s = aux xs e1 ++
              List.fold_left (fun acc (_,(p,ei)) ->
                let ys = vars_of_p p in
                let xs' = xs++ys in
                (acc ++ aux xs' ei)) SMap.empty hs in
      (match eo with
      | None -> s
      | Some ew -> s ++ aux xs ew)
  | E_letIn(p,_,e1,e2) ->
      let ys = vars_of_p p in
      let xs' = xs ++ ys in
      aux xs e1 ++ aux xs' e2
  | E_app(e1,e2) ->
      aux xs e1 ++ aux xs e2
  | E_fun(p,_,e1) ->
      let ys = vars_of_p p in
      let xs' = xs++ys in
      aux xs' e1
  | E_fix(f,(p,_,e)) ->
      let ys = vars_of_p p in
      let xs' = SMap.add f () @@ (xs++ys) in
      aux xs' e
  | E_tuple(es) ->
      fv_list xs es
  | E_reg((p,_,e1), e0, _) ->
      let ys = vars_of_p p in
      let xs' = xs++ys in
      aux xs' e1 ++ aux xs e0
  | E_exec(e1,e2,eo,_k) ->
      (* _k is in a different name space than variables *)
      aux xs e1 ++ aux xs e2 ++ (match eo with 
                                 | None -> SMap.empty 
                                 | Some e3 -> aux xs e3)
  | E_record(b_list) ->
      fv_list xs (List.map snd b_list)
  | E_record_field(e1,x,_) ->
      aux xs e1
  | E_record_update(e1,x2,e2,_) ->
      aux xs e1 ++ aux xs e2
  | E_ref(e1) ->
      aux xs e1
  | E_get(e1) ->
      if get_arrays then aux xs e1 else SMap.empty
  | E_set(e1,e2) ->
      (if get_arrays then aux xs e1 else SMap.empty) 
      ++ aux xs e2
  | E_array_make(_,e1,_) ->
      aux xs e1
  | E_array_create _ ->
      SMap.empty
  | E_array_length(x,_) ->
      if get_arrays 
      then fv_var xs x
      else SMap.empty
  | E_array_get((x,_),e1) ->
      let vs = aux xs e1 in
      if get_arrays 
      then fv_var xs x ++ vs 
      else vs
  | E_array_set((x,_),e1,e2) ->
      let vs = aux xs e1 ++ aux xs e2 in
      if get_arrays 
      then fv_var xs x ++ vs 
      else vs
  | E_array_get_start((x,_),e1) ->
      let vs = aux xs e1 in
      if get_arrays 
      then fv_var xs x ++ vs 
      else vs
  | E_array_get_end _ ->
      SMap.empty
  | E_array_set_immediate((x,_),e1,e2) ->
      let vs = aux xs e1 ++ aux xs e2 in
      if get_arrays 
      then fv_var xs x ++ vs 
      else vs
  | E_array_from_file(x,e1) ->
      let vs = aux xs e1 in
      if get_arrays 
      then fv_var xs x ++ vs 
      else vs
  | E_par(es) ->
      fv_list xs es
  | E_for(i,e1,e2,e3,_,_) ->
      let vs = aux xs e1 ++ aux xs e2 in
      let xs' = SMap.add i () @@ xs in
      vs ++ aux xs' e3 
  | E_parfor(i,_,_,e,_) ->
        (let xs' = SMap.add i () @@ xs in
        aux xs' e)
  | E_generate((p,_,e1),e2,_,_,_) ->
      let ys = vars_of_p p in
      let xs' = xs++ys in
      aux xs' e1 ++ aux xs e2
  | E_vector(es) ->
      fv_list xs es
  | E_vector_mapi(_,(p,_,e1),e2,_) ->
      let ys = vars_of_p p in
      let xs' = xs++ys in
      aux xs' e1 ++ aux xs e2
  | E_run(_x,e,_) -> aux xs e (* what about _x ? *)
  | E_pause (_,e) -> aux xs e
  | E_sig_get(x) -> 
      if get_sig then fv_var xs x else SMap.empty
  | E_emit(x,e1) ->
      let vs = aux xs e1 in
      if get_sig then vs ++ fv_var xs x else vs
  | E_sig_create(e1) ->
      aux xs e1
  | E_loop(e1) ->
      aux xs e1
  | E_trap _ -> SMap.empty
  | E_exit(x,e1) ->
      (if get_sig then fv_var xs x else SMap.empty) ++ aux xs e1
  | E_suspend(e1,x) ->
      (if get_sig then fv_var xs x else SMap.empty) ++ aux xs e1
  | E_assert(e1,_) ->
      aux xs e1
  in
  aux xs e

let fv_arrays ?(xs=SMap.empty) e =
  let open Ast in
  let rec fv_list xs es =
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
      fv_list xs (List.map snd hs) ++
      aux xs e_els
  | E_match(e1,hs,eo) ->
      let s = aux xs e1 ++
              List.fold_left (fun acc (_,(p,ei)) ->
                let ys = vars_of_p p in
                let xs' = xs++ys in
                (acc ++ aux xs' ei)) SMap.empty hs in
      (match eo with
      | None -> s
      | Some ew -> s ++ aux xs ew)
  | E_letIn(p,_,e1,e2) ->
      let ys = vars_of_p p in
      let xs' = xs ++ ys in
      aux xs e1 ++ aux xs' e2
  | E_app(e1,e2) ->
      aux xs e1 ++ aux xs e2
  | E_fun(p,_,e1) ->
      let ys = vars_of_p p in
      let xs' = xs++ys in
      aux xs' e1
  | E_fix(f,(p,_,e)) ->
      let ys = vars_of_p p in
      let xs' = SMap.add f () @@ (xs++ys) in
      aux xs' e
  | E_tuple(es) ->
      fv_list xs es
  | E_reg((p,_,e1), e0, _) ->
      let ys = vars_of_p p in
      let xs' = xs++ys in
      aux xs' e1 ++ aux xs e0
  | E_exec(e1,e2,eo,_k) ->
      (* _k is in a different name space than variables *)
      aux xs e1 ++ aux xs e2 ++ (match eo with 
                                 | None -> SMap.empty 
                                 | Some e3 -> aux xs e3)
  | E_record(b_list) ->
      fv_list xs (List.map snd b_list)
  | E_record_field(e1,x,_) ->
      aux xs e1
  | E_record_update(e1,x2,e2,_) ->
      aux xs e1 ++ aux xs e2
  | E_ref(e1) ->
      aux xs e1
  | E_get(e1) ->
      aux xs e1
  | E_set(e1,e2) ->
      aux xs e1
      ++ aux xs e2
  | E_array_make(_,e1,_) ->
      aux xs e1
  | E_array_create _ ->
      SMap.empty
  | E_array_length(x,_) ->
      fv_var xs x
  | E_array_get((x,_),e1) ->
      let vs = aux xs e1 in
      vs ++ fv_var xs x
  | E_array_set((x,_),e1,e2) ->
      let vs = aux xs e1 ++ aux xs e2 in
      vs ++ fv_var xs x
  | E_array_get_start((x,_),e1) ->
      let vs = aux xs e1 in
      vs ++ fv_var xs x
  | E_array_get_end _ ->
      SMap.empty
  | E_array_set_immediate((x,_),e1,e2) ->
      let vs = aux xs e1 ++ aux xs e2 in
     vs ++ fv_var xs x
  | E_array_from_file(x,e1) ->
      let vs = aux xs e1 in
      vs ++ fv_var xs x
  | E_par(es) ->
      fv_list xs es
  | E_for(i,e1,e2,e3,_,_) ->
      let vs = aux xs e1 ++ aux xs e2 in
      let xs' = SMap.add i () @@ xs in
      vs ++ aux xs' e3
  | E_parfor(i,_,_,e,_) ->
      let xs' = SMap.add i () @@ xs in
      aux xs' e
  | E_generate((p,_,e1),e2,_,_,_) ->
      let ys = vars_of_p p in
      let xs' = xs++ys in
      aux xs' e1 ++ aux xs e2
  | E_vector(es) ->
      fv_list xs es
  | E_vector_mapi(_,(p,_,e1),e2,_) ->
      let ys = vars_of_p p in
      let xs' = xs++ ys in
      aux xs' e1 ++ aux xs e2
  | E_run(_x,e1,_) -> (* what about _x ? *)
      aux xs e1
  | E_pause (_,e) -> aux xs e
  | E_sig_get _ -> 
      SMap.empty
  | E_emit(_,e1) ->
      aux xs e1
  | E_sig_create(e1) ->
      aux xs e1
  | E_loop(e1) ->
      aux xs e1
  | E_trap _ -> SMap.empty
  | E_exit(x,e1) ->
      aux xs e1
  | E_suspend(e1,x) ->
      aux xs e1
  | E_assert(e1,_) ->
      aux xs e1
  in
  aux xs e


