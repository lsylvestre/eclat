open Ast
open Ast_subst

open Pattern

(**

  performs ANF + let-floating + copy propagation
  
  expression in ANF-form are defined as follows:

  e ::=
        xc
      | fun x -> e
      | fix f (fun x -> e)
      | xc xc
      | if xc then e else e
      | match xc with c -> e | ... | _ -> e end
      | match xc with inj p -> e | ... | {_ -> e}? end
      | reg_l (fun p -> e) last xc
      | exec_l e xc
      | let f = fun x -> e in e
      | let f = fix f' (fun x -> e) in e
      | let x = e in e
      

      | ref e         ;; references
      | x := xs
      | !x
      
      | x[xc]
      | x.length
      | x[xc] <- xc

  xc := (xc,xc ... xc)
      | c
      | x
      | x.length
      | x.size
      | fst x | snd x | ...
*)

let rec is_xc (e:e) : bool =
  match un_deco e with
  | E_deco(e,_) -> is_xc e
  | E_var _ | E_const _ -> true
  | E_tuple(es) -> List.for_all is_xc es
  | E_app(E_const(Op(GetTuple _)),e) -> is_xc e
  | E_array_length _ | E_matrix_size _ -> true
  | _ -> false

(** [plug e ctx] plugs expression [e] into context [ctx], i.e., returns
   [ctx e] if [is_xc e] is true, otherwise [let y = e in ctx y]
   with [y] a fresh name. *)
let plug (e:e) (context: e -> e) : e =
  if is_xc e then context e else
  let x = gensym () in
  E_letIn(P_var x,e,context (E_var x))

(** [plug_n es ctx] plugs expresions [es] into context [ctx]
    from left to right *)
let plug_n (es:e list) (context : e list -> e) : e =
  let rec plug_n_aux aas es context =
    match es with
    | [] -> context (List.rev aas)
    | e::es' -> plug e @@ fun v -> plug_n_aux (v::aas) es' context
  in
  plug_n_aux [] es context

(** [anf e] puts expression [e] in ANF-form *)
let rec anf (e:e) : e =
  let rec glob (e:e) : _ * e =
  match e with
  | E_deco _ ->
      Ast_undecorated.still_decorated e
  | E_var _ | E_const _ -> [],e
  | E_fun(x,e1) ->
      [],E_fun(x,anf e1)
  | E_fix(f,(x,e1)) ->
      [],E_fix(f,(x,anf e1))
  | E_if(e1,e2,e3) ->
      let ds1,e1' = glob e1 in
      ds1, plug e1' @@ fun xc1 ->
      E_if(xc1,anf e2,anf e3)
  | E_case(e1,hs,e_els) ->
      let ds1,e1' = glob e1 in
      ds1,plug e1' @@ fun xc ->
      E_case(xc,List.map (fun (c,e) -> c,anf e) hs,anf e_els)
  | E_match(e1,hs,eo) ->
      let ds1,e1' = glob e1 in
      ds1,plug (anf e1') @@ fun xc ->
      E_match(xc,List.map (fun (x,(p,e)) -> x,(p,anf e)) hs,Option.map anf eo)
 (* | E_letIn(P_var f,(E_fix(g,(p,e1))),e2) when f <> g ->
      assert (not (pat_mem f p) && not (pat_mem g p));
      anf @@ E_letIn(P_var f,E_fix(f,(p,subst_e g (E_var f) e1)),e2)*)
  | E_letIn(P_tuple ps,E_tuple es,e2) ->
      glob (List.fold_right2 (fun p e acc -> E_letIn(p,e,acc)) ps es e2)
  | E_letIn(p,e1,e2) ->
      let ds1,e1' = glob e1 in
      let e' = anf e2 in
      ds1,((*match p,e1' with 
           | P_unit,E_const Unit -> e'
           | _ -> if is_xc e1' then subst_p_e p e1' e' else*)   (* bug subst_p_e ici ! *)
           E_letIn(p,e1',e'))
  | E_tuple(es) ->
      let dss,es' = List.split (List.map glob es) in
      List.concat dss, 
      plug_n es' @@ fun xs -> E_tuple(xs)
  | E_app(E_const _ as ec,e2) ->
      let ds2,e2' = glob e2 in
      ds2,plug e2' @@ fun x2 ->
      E_app(ec,x2)
  | E_app(e1,e2) ->
      let ds1,e1' = glob e1 in
      let ds2,e2' = glob e2 in
      ds1@ds2,plug e1' @@ fun xc1 ->
      plug e2' @@ fun xc2 ->
      E_app(xc1,xc2)
  | E_reg((p,e1),e0,l) ->
      let ds0,e0' = glob e0 in
      ds0,plug e0' @@ fun xc0 ->
      E_reg((p,anf e1),xc0,l)
  | E_exec(e1,e0,eo,l) ->
      let ds0,e0' = glob e0 in
      let ds3,eo' = match eo with 
                    | None -> [],eo 
                    | Some e3 -> let ds3,e3' = glob e3 in
                    ds3,Some e3 in
      ds0@ds3,plug e0' @@ fun xc0 ->
      (match eo' with
       | None -> E_exec(anf e1,xc0,None,l)
       | Some e3' -> plug e3' @@ fun xc3 -> E_exec(anf e1,xc0,Some xc3,l))
  | E_ref(e1) ->
      let ds1,e1' = glob e1 in
      ds1,plug e1' @@ fun xc1 ->
      E_ref(xc1)
  | E_get(e1) ->
      let ds1,e1' = glob e1 in
      ds1,plug e1' @@ fun xc1 ->
      E_get(xc1)
  | E_set(e1,e2) ->
      let ds1,e1' = glob e1 in
      let ds2,e2' = glob e2 in
      ds1@ds2,plug e1' @@ fun xc1 ->
      plug e2' @@ fun xc2 ->
      E_set(xc1,xc2)
  | E_array_length _ ->
      [],e
  | E_local_static_array(e1,loc) ->
      let ds1,e1' = glob e1 in
      ds1,plug e1' @@ fun xc1 ->
              E_local_static_array(xc1,loc)
  | E_array_get(x,e1) ->
      let ds1,e1' = glob e1 in
      ds1,plug e1' @@ fun xc1 ->
          E_array_get(x,xc1)
  | E_array_set(x,e1,e2) ->
      let ds1,e1' = glob e1 in
      let ds2,e2' = glob e2 in
      ds1@ds2,plug e1' @@ fun xc1 ->
              plug e2' @@ fun xc2 ->
              E_array_set(x,xc1,xc2)
  | E_local_static_matrix(e1,es,loc) ->
      let ds1,e1' = glob e1 in
      let dss,es' = List.split (List.map glob es) in
      List.concat dss@ds1,
      plug e1' @@ fun xc1 ->
      plug_n es' @@ fun xs ->
      E_local_static_matrix(xc1,xs,loc)
  | E_matrix_size _ ->
      [],e
  | E_matrix_get(x,es) ->
      let dss,es' = List.split (List.map glob es) in
      List.concat dss,
      plug_n es' @@ fun xs -> E_matrix_get(x,xs)
  | E_matrix_set(x,es,e2) ->
      let dss,es' = List.split (List.map glob es) in
      let ds2,e2' = glob e2 in
      List.concat dss@ds2,
      plug_n es' @@ fun xs ->
      plug e2' @@ fun xc2 ->
      E_matrix_set(x,xs,xc2)
  | E_par(es) ->
      [],E_par(List.map anf es)
  (*| E_absLabel(l,e1) ->
      E_absLabel(l,anf e1)
  | E_appLabel(e1,l,lc) ->
      E_appLabel(anf e1,l,lc)*)
  | E_for(x,e_st1,e_st2,e3,loc) ->
      [],E_for(x,anf e_st1,anf e_st2,anf e3,loc)
      (* NB: [e_st1] and [e_st2] are *not* moved up with `plug` *)
  | E_generate((p,e1),e2,e_st3,loc) ->
      [],plug (anf e2) @@ fun xc ->
      E_generate((p,anf e1),xc,anf e_st3,loc) 
      (* NB: [e_st3] is *not* moved up with `plug` 
*)
  | E_vector(es) ->
      let dss,es' = List.split (List.map glob es) in
      List.concat dss, 
      plug_n es' @@ fun xs -> E_vector(xs)
  | E_vector_mapi(is_par,(p,e1),e2,ty) ->
      [],plug (anf e2) @@ fun xc ->
      E_vector_mapi(is_par,(p,anf e1),xc,ty) 
  | E_int_mapi(is_par,(p,e1),e2,ty) ->
      [],plug (anf e2) @@ fun xc ->
      E_int_mapi(is_par,(p,anf e1),xc,ty) 
  in 
  let ds,e' = glob e in 
  Ast_mapper.declare ds e'

(** [anf e] puts program [pi] in ANF-form *)
let anf_pi (pi:pi) : pi =
  { pi with main = anf pi.main } (*
  { pi with main = anf pi.main }*)

