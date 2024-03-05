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
      | fst x | snd x | ...
*)

let rec is_xc (e:e) : bool =
  match un_deco e with
  | E_deco(e,_) -> is_xc e
  | E_var _ | E_const _ -> true
  | E_tuple(es) -> List.for_all is_xc es
  | E_app(E_const(Op(GetTuple _)),e) -> is_xc e
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

(*************(** [anf e] puts expression [e] in ANF-form *)
let rec anf (e:e) : e =
  match e with
  | E_deco _ ->
      Ast_undecorated.still_decorated e
  | E_var _ | E_const _ -> e
  | E_fun(x,e1) ->
      E_fun(x,anf e1)
  | E_fix(f,(x,e1)) ->
      E_fix(f,(x,anf e1))
  | E_if(e1,e2,e3) ->
      plug (anf e1) @@ fun xc ->
      E_if(xc,anf e2,anf e3)
  | E_case(e,hs,e_els) ->
      plug (anf e) @@ fun xc ->
      E_case(xc,List.map (fun (c,e) -> c,anf e) hs,anf e_els)
  | E_match(e,hs,eo) ->
      plug (anf e) @@ fun xc ->
      E_match(xc,List.map (fun (x,(p,e)) -> x,(p,anf e)) hs,Option.map anf eo)
  | E_letIn(P_var f,(E_fix(g,(p,e1))),e2) when f <> g ->
      assert (not (pat_mem f p) && not (pat_mem g p));
      anf @@ E_letIn(P_var f,E_fix(f,(p,subst_e g (E_var f) e1)),e2)
  | E_letIn(P_var x,(E_var _ as e1),e2) ->
      anf @@ subst_e x e1 e2
  | E_letIn(p,e1,e2) ->
      E_letIn(p,anf e1,anf e2)
  | E_tuple(es) ->
      plug_n (List.map anf es) @@
      fun xs -> E_tuple(xs)
  | E_app(E_const _ as ec,e2) ->
      plug (anf e2) @@ fun x2 ->
      E_app(ec,x2)
  | E_app(e1,e2) ->
      plug (anf e1) @@ fun xc1 ->
      plug (anf e2) @@ fun xc2 ->
      E_app(xc1,xc2)
  | E_reg((p,e1),e0,l) ->
      plug (anf e0) @@ fun xc0 ->
      E_reg((p,anf e1),xc0,l)
    | E_exec(e1,e0,l) ->
        plug (anf e0) @@ fun xc0 ->
        E_exec(anf e1,xc0,l)
  | E_set(e1,e2) ->
      plug (anf e1) @@ fun xc1 ->
      plug (anf e2) @@ fun xc2 ->
      E_set(xc1,xc2)
  | E_array_length _ ->
      e
  | E_array_get(x,e1) ->
      plug (anf e1) @@ fun xc1 ->
      E_array_get(x,xc1)
  | E_array_set(x,e1,e2) ->
      plug (anf e1) @@ fun xc1 ->
      plug (anf e2) @@ fun xc2 ->
      E_array_set(x,xc1,xc2)
  | E_matrix_size _ ->
      e
  | E_matrix_get(x,es) ->
      plug_n (List.map anf es) @@ fun xs ->
      E_matrix_get(x,xs)
  | E_matrix_set(x,es,e2) ->
      plug_n (List.map anf es) @@ fun xs -> 
      plug (anf e2) @@ fun xc2 ->
      E_matrix_set(x,xs,xc2)
  | E_par(es) ->
      E_par(List.map anf es)
  | E_absLabel(l,e1) ->
      E_absLabel(l,anf e1)
  | E_appLabel(e1,l,lc) ->
      E_appLabel(anf e1,l,lc)
  | E_for(x,e_st1,e_st2,e3,loc) ->
      E_for(x,anf e_st1,anf e_st2,anf e3,loc)
      (* NB: [e_st1] and [e_st2] are *not* moved up with `plug` *)
  | E_generate((p,e1),e2,e_st3,loc) ->
      plug (anf e2) @@ fun xc ->
      E_generate((p,anf e1),xc,anf e_st3,loc) 
      (* NB: [e_st3] is *not* moved up with `plug` *)
*)



(** [anf e] puts expression [e] in ANF-form *)
let rec anf2 (e:e) : e =
  let rec glob (e:e) : _ * e =
  match e with
  | E_deco _ ->
      Ast_undecorated.still_decorated e
  | E_var _ | E_const _ -> [],e
  | E_fun(x,e1) ->
      [],E_fun(x,anf2 e1)
  | E_fix(f,(x,e1)) ->
      [],E_fix(f,(x,anf2 e1))
  | E_if(e1,e2,e3) ->
      let ds1,e1' = glob e1 in
      ds1, plug e1' @@ fun xc1 ->
      E_if(xc1,anf2 e2,anf2 e3)
  | E_case(e1,hs,e_els) ->
      let ds1,e1' = glob e1 in
      ds1,plug e1' @@ fun xc ->
      E_case(xc,List.map (fun (c,e) -> c,anf2 e) hs,anf2 e_els)
  | E_match(e1,hs,eo) ->
      let ds1,e1' = glob e1 in
      ds1,plug (anf2 e1') @@ fun xc ->
      E_match(xc,List.map (fun (x,(p,e)) -> x,(p,anf2 e)) hs,Option.map anf2 eo)
 (* | E_letIn(P_var f,(E_fix(g,(p,e1))),e2) when f <> g ->
      assert (not (pat_mem f p) && not (pat_mem g p));
      anf @@ E_letIn(P_var f,E_fix(f,(p,subst_e g (E_var f) e1)),e2)*)
  | E_letIn(P_tuple ps,E_tuple es,e2) ->
      glob (List.fold_right2 (fun p e acc -> E_letIn(p,e,acc)) ps es e2)
  | E_letIn(p,e1,e2) ->
      let ds1,e1' = glob e1 in
      let e' = anf2 e2 in
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
      E_reg((p,anf2 e1),xc0,l)
  | E_exec(e1,e0,l) ->
      let ds0,e0' = glob e0 in
      ds0,plug e0' @@ fun xc0 ->
      E_exec(anf2 e1,xc0,l) 
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
      [],E_par(List.map anf2 es)
  (*| E_absLabel(l,e1) ->
      E_absLabel(l,anf e1)
  | E_appLabel(e1,l,lc) ->
      E_appLabel(anf e1,l,lc)*)
  | E_for(x,e_st1,e_st2,e3,loc) ->
      [],E_for(x,anf2 e_st1,anf2 e_st2,anf2 e3,loc)
      (* NB: [e_st1] and [e_st2] are *not* moved up with `plug` *)
  | E_generate((p,e1),e2,e_st3,loc) ->
      [],plug (anf2 e2) @@ fun xc ->
      E_generate((p,anf2 e1),xc,anf2 e_st3,loc) 
      (* NB: [e_st3] is *not* moved up with `plug` 
*)
  in 
  let ds,e' = glob e in 
  Ast_mapper.declare ds e'

(** [anf e] puts program [pi] in ANF-form *)
let anf_pi (pi:pi) : pi =
  { pi with main = anf2 pi.main } (*
  { pi with main = anf pi.main }*)

