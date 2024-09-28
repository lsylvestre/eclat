open Ast

(** traversal order of sub-expressions is unspecified *)

let rec map f e =
  match e with
  | E_deco(e, ty) ->
      let e' = f e in
      E_deco(e', ty)
  | E_const _ | E_var _ -> e
  | E_fun(p, ty, e) ->
      E_fun(p, ty, f e)
  | E_fix(x, (p, ty, e)) ->
      let e' = f e in
      E_fix(x, (p, ty, e'))
  | E_tuple es ->
      let es' = List.map f es in
      E_tuple es'
  | E_app(e1, e2) ->
      let e1' = f e1 in
      let e2' = f e2 in
      E_app(e1', e2')
  | E_if(e1, e2, e3) ->
      let e1' = f e1 in
      let e2' = f e2 in
      let e3' = f e3 in
      E_if(e1', e2', e3')
  | E_case(e1, hs, e_els) ->
      let e1' = f e1 in
      let hs' = List.map (fun (c, e) -> c, f e) hs in
      let e_els' = f e_els in
      E_case(e1',hs',e_els')
  | E_match(e1, hs, eo) ->
      let e1' = f e1 in
      let hs' = List.map (fun (c, (p, e)) -> c, (p, f e)) hs in
      let eo' = Option.map f eo in
      E_match(e1', hs', eo')
  | E_letIn(p, ty, e1, e2) ->
      let e1' = f e1 in
      let e2' = f e2 in
      E_letIn(p, ty, e1', e2')
  | E_ref(e1) ->
      let e1' = f e1 in
      E_ref(e1')
  | E_get(e1) ->
      let e1' = f e1 in
      E_get(e1')
  | E_set(e1,e2) ->
      let e1' = f e1 in
      let e2' = f e2 in
      E_set(e1',e2')
  | E_array_make(sz,e1,loc) ->
      let e1' = f e1 in
      E_array_make(sz,e1',loc)
  | E_array_create _ ->
      e
  | E_array_length _ ->
      e
  | E_array_get(x, e1) ->
      let e1' = f e1 in
      E_array_get(x,e1')
  | E_array_set(x,e1,e2) ->
      let e1' = f e1 in
      let e2' = f e2 in
      E_array_set(x, e1', e2')
  | E_par(es) ->
      let es' = List.map f es in
      E_par es'
  | E_reg((p,tyB,e1),e0,l) ->
      let e0' = f e0 in
      let e1' = f e1 in
      E_reg((p, tyB, e1'), e0', l)
  | E_exec(e1,e2,e3,l) ->
      let e1' = f e1 in
      let e2' = f e2 in
      let e3' = Option.map f e3 in
      E_exec(e1', e2', e3', l)
  | E_vector es ->
      let es' = List.map f es in
      E_vector es'
  | E_vector_mapi (is_par, (p, typ, e1), e2, ty) ->
      let e1' = f e1 in
      let e2' = f e2 in
      E_vector_mapi (is_par, (p, typ, e1'), e2', ty)
  | E_run(i,e) ->
      E_run(i, f e)
  | E_for(x,e_st1,e_st2,e,loc) ->
      E_for(x,f e_st1,f e_st2,f e,loc)
  | E_generate((p, ty, e1), e2, e_st1, loc) ->
      E_generate((p, ty, f e1), f e2, f e_st1, loc)
(** traversal order of sub-expressions is unspecified *)

let rec iter f (e:e) : unit =
  match e with
  | E_deco (e,_) ->
      f e
  | E_var _ ->
      ()
  | E_const _ ->
      ()
  | E_if(e1,e2,e3) ->
      f e1; f e2; f e3
  | E_case(e1,hs,e_els) ->
      f e1; List.iter (fun (_,ei) -> f ei) hs; f e_els
  | E_match(e1,hs,eo) ->
      f e1; List.iter (fun (_,(_,ei)) -> f ei) hs; Option.iter f eo
  | E_app(e1,e2) ->
      f e1; f e2
  | E_letIn(_,_,e1,e2) ->
      f e1; f e2
  | E_tuple es ->
      List.iter f es
  | E_fun(_,_,e) | E_fix(_,(_,_,e)) ->
      f e
  | E_ref(e1) ->
      f e1
  | E_get(e1) -> f e1
  | E_set(e1,e2) ->
      f e1; f e2
  | E_par(es) ->
      List.iter f es
   | E_reg((_,_,e1),e0,_) ->
      f e1; f e0
  | E_exec(e1,e2,e3,_) ->
      f e1; f e2; Option.iter f e3
  | E_array_make(_,e1,_) ->
      f e1
  | E_array_create _ ->
      ()
  | E_array_length _ ->
      ()
  | E_array_get(_,e1) ->
      f e1
  | E_array_set(_,e1,e2) ->
      f e1; f e2
  | E_for(_,_,_,e,_) ->
      f e
  | E_generate((_,_,e1),e2,e_st3,_) ->
      f e1; f e2; f e_st3
  | E_vector es ->
      List.iter f es
  | E_vector_mapi(_,(_,_,e1),e2,_) ->
      f e1; f e2
  | E_run(_,e1) ->
      f e1

let declare ds ts e =
  List.fold_right2 (fun (x,v) t e -> E_letIn(P_var x,t,v,e)) ds ts e

let accum f (e:e) =   (* : ((x * ty * e) list * e)*)
  let rec aux e =
    let open Ast in
      let aux_list es =
        let rec loop dss es_acc es =
          match es with
          | [] -> List.concat (List.rev dss), List.rev es_acc
          | ei::es' ->
             let (dsi,ei') = aux ei in
             loop (dsi::dss) (ei'::es_acc) es'
      in loop [] [] es
    in
    match f aux e with
    | Some (ds,e') -> (ds,e')
    | None ->
        (match e with
        | E_deco(e1,deco) ->
            let ds1,e1' = aux e1 in
            ds1, E_deco(e1',deco)
        | E_const _ | E_var _ -> [],e
        | E_tuple es ->
            let ds,es' = aux_list es in
            ds,E_tuple(es')
        | E_app(e1,e2) ->
            let ds1,e1' = aux e1 in
            let ds2,e2' = aux e2 in
            ds1@ds2,E_app(e1',e2')
        | E_letIn(p,ty,e1,e2) ->
            let ds1,e1' = aux e1 in
            let ds2,e2' = aux e2 in
            ds1@ds2,E_letIn(p,ty,e1',e2')
        | E_fix(f,(p,ty,e1)) ->
            let ds1,e1' = aux e1 in
            let v = E_fix(f,(p,ty,e1')) in
            ds1,v
        | E_fun(p,ty,e1) ->
            let ds1,e1' = aux e1 in
            ds1,E_fun(p,ty,e1')
        | E_if(e1,e2,e3) ->
            let ds1,e1' = aux e1 in
            let ds2,e2' = aux e2 in
            let ds3,e3' = aux e3 in
            ds1@ds2@ds3,E_if(e1',e2',e3')
        | E_case(e1,hs,e_els) ->
          let ds1,e1' = aux e1 in
          let dss,hs' = List.split @@ List.map (fun (c,e) -> let ds,e' = aux e in ds,(c,e')) hs in
          let ds,e_els' = aux e_els in
          ds1@List.concat dss@ds, E_case(e1',hs',e_els')
        | E_match(e1,hs,eo) ->
          let ds1,e1' = aux e1 in
          let dss,hs' = List.split @@ List.map (fun (x,(p,e)) -> let ds,e' = aux e in ds,(x,(p,e'))) hs in
          let dsw,eo' = match eo with
                        | None -> [],eo
                        | Some ew -> let dsw,ew' = aux ew in
                                     (dsw,Some ew')
          in
          ds1@List.concat dss@dsw, E_match(e1',hs',eo')
        | E_ref(e1) ->
            let ds1,e1' = aux e1 in
            ds1,E_ref(e1')
        | E_get _ ->
            [], e
        | E_set(e1,e2) ->
            let ds1,e1' = aux e1 in
            let ds2,e2' = aux e2 in
            ds1@ds2,E_set(e1',e2')
        | E_array_make(sz,e1,loc) ->
            let ds1,e1' = aux e1 in
            ds1,E_array_make(sz,e1',loc)
        | E_array_create _ ->
            [],e
        | E_array_length _ ->
            [],e
        | E_array_get(x,e1) ->
            let ds1,e1' = aux e1 in
            ds1,E_array_get(x,e1')
        | E_array_set(x,e1,e2) ->
            let ds1,e1' = aux e1 in
            let ds2,e2' = aux e2 in
            ds1@ds2,E_array_set(x,e1',e2')
        | E_par(es) ->
            let ds,es' = aux_list es in
            ds,E_par(es')
        | E_reg((p,tyB,e1),e0,l) ->
            let ds1,e1' = aux e1 in
            let ds0,e0' = aux e0 in
            ds1@ds0,E_reg((p,tyB,e1'),e0',l)
        | E_exec(e1,e2,eo,l) ->
            let ds1,e1' = aux e1 in
            let ds2,e2' = aux e2 in
            let ds3,eo' = match eo with
                          | None -> [],None 
                          | Some e3 -> let ds,e3' = aux e3 in
                                       ds,Some e3' in
            ds2@ds1@ds3,E_exec(e1',e2',eo',l)
        | E_for(x,e_st1,e_st2,e3,loc) ->
            let ds1,e_st1' = aux e_st1 in
            let ds2,e_st2' = aux e_st2 in
            let ds3,e3' = aux e3 in
            ds1@ds2@ds3,E_for(x,e_st1',e_st2',e3,loc)
             (* NB: definitions in [e_st1] and [e_st2] and [e3]
                are *not* globalized *)
        | E_generate((p,ty,e1),e2,e_st3,loc) ->
          let ds1,e1' = aux e1 in
          let ds2,e2' = aux e2 in
          let ds3,e_st3' = aux e_st3 in
          ds1@ds2@ds3,E_generate((p,ty,e1'),e2',e_st3',loc)
          (* NB: definitions in [e_st1] are *not* globalized *)
        | E_vector es ->
            let ds,es' = aux_list es in
            ds,E_vector(es')
        | E_vector_mapi(is_par,(p,typ,e1),e2,ty) ->
            let ds1,e1' = aux e1 in
            let ds2,e2' = aux e2 in
            ds1@ds2,E_vector_mapi(is_par,(p,typ,e1'),e2',ty)

        | E_run(x,e1) ->
            let ds1,e1' = aux e1 in
            ds1,E_run(x,e1')
  ) 
  in 
  aux e


let map_pi f pi =
  Map_pi.map (map f) pi

