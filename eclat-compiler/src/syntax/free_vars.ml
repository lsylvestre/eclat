
open Ast

let fv_var xs x =
  if SMap.mem x xs then SMap.empty else SMap.singleton x ()

let fv ?(get_arrays=true) ?(xs=SMap.empty) e =
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
  | E_array_length(x) ->
      if get_arrays 
      then fv_var xs x
      else SMap.empty
  | E_array_get(x,e1) ->
      let vs = aux xs e1 in
      if get_arrays 
      then fv_var xs x ++ vs 
      else vs
  | E_array_set(x,e1,e2) ->
      let vs = aux xs e1 ++ aux xs e2 in
      if get_arrays 
      then fv_var xs x ++ vs 
      else vs
  | E_par(es) ->
      fv_list xs es
  | E_for(i,e_st1,e_st2,e,_) ->
      aux xs e_st1 ++ aux xs e_st2 ++
        (let xs' = SMap.add i () @@ xs in
        aux xs' e)
  | E_generate((p,_,e1),e2,e_st3,_) ->
      let ys = vars_of_p p in
      let xs' = xs++ys in
      aux xs' e1 ++ aux xs e2 ++ aux xs e_st3
  | E_vector(es) ->
      fv_list xs es
  | E_vector_mapi(_,(p,_,e1),e2,_) ->
      let ys = vars_of_p p in
      let xs' = xs++ys in
      aux xs' e1 ++ aux xs e2
  | E_run(_x,e) -> aux xs e (* what about _x ? *)
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
  | E_array_length(x) ->
      SMap.singleton x () 
  | E_array_get(x,e1) ->
      let vs = aux xs e1 in
      SMap.add x () vs 
  | E_array_set(x,e1,e2) ->
      let vs = aux xs e1 ++ aux xs e2 in
      SMap.add x () vs 
  | E_par(es) ->
      fv_list xs es
  | E_for(i,e_st1,e_st2,e,_) ->
      aux xs e_st1 ++ aux xs e_st2 ++
        (let xs' = SMap.add i () @@ xs in
        aux xs' e)
  | E_generate((p,_,e1),e2,e_st3,_) ->
      let ys = vars_of_p p in
      let xs' = xs++ys in
      aux xs' e1 ++ aux xs e2 ++ aux xs e_st3
  | E_vector(es) ->
      fv_list xs es
  | E_vector_mapi(_,(p,_,e1),e2,_) ->
      let ys = vars_of_p p in
      let xs' = xs++ys in
      aux xs' e1 ++ aux xs e2
  | E_run(_x,e1) -> (* what about _x ? *)
      aux xs e1
  in
  aux xs e

