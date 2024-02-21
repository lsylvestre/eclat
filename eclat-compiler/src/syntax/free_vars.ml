
open Ast

let fv_var xs x =
  if SMap.mem x xs then SMap.empty else SMap.singleton x ()

let fv ?(get_arrays=true) ?(xs=SMap.empty) e =
  let open Ast in
  let rec aux xs = function
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
      List.fold_left (fun acc (c,ei) -> (acc ++ aux xs ei)) SMap.empty hs ++
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
  | E_letIn(p,e1,e2) ->
      let ys = vars_of_p p in
      let xs' = xs ++ ys in
      aux xs e1 ++ aux xs' e2
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
      List.fold_left (fun acc ei -> acc ++ aux xs ei) SMap.empty es
  | E_reg((p,e1), e0, _) ->
      let ys = vars_of_p p in
      let xs' = xs++ys in
      aux xs' e1 ++ aux xs e0
  | E_exec(e1,e2,_k) ->
      (* _k is in a different name space than variables *)
      aux xs e1 ++ aux xs e2
   | E_lastIn(x,e1,e2) ->
      let xs' = SMap.add x () xs in (* todo *)
      aux xs e1 ++ aux xs' e2
  | E_set(x,e1) ->
      let vs = fv_var xs x ++ aux xs e1 in
      if get_arrays 
      then SMap.add x () vs 
      else vs
  | E_static_array_get(x,e1) ->
      let vs = aux xs e1 in
      if get_arrays 
      then SMap.add x () vs 
      else vs
  | E_static_array_length(x) ->
      if get_arrays 
      then SMap.singleton x () 
      else SMap.empty
  | E_static_array_set(x,e1,e2) ->
      let vs = aux xs e1 ++ aux xs e2 in
      if get_arrays 
      then SMap.add x () vs 
      else vs
  | E_par(e1,e2) ->
      aux xs e1 ++ aux xs e2
  | E_absLabel(l,e1) ->
      let xs' = SMap.add l () @@ xs in
      aux xs' e1
  | E_appLabel(e1,_,l) ->
      aux xs e1
  | E_for(i,e_st1,e_st2,e,_) ->
      aux xs e_st1 ++ aux xs e_st2 ++
        (let xs' = SMap.add i () @@ xs in
        aux xs' e)
  | E_generate((p,e1),e2,e_st3,_) ->
      let ys = vars_of_p p in
      let xs' = xs++ys in
      aux xs' e1 ++ aux xs e2 ++ aux xs e_st3
  in
  aux xs e
