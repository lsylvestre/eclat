open Ast
open Ast_subst

open Pattern

(**

  expression in ANF-form are defined as follows:

  e ::=
        xc
      | fun x -> e
      | fix f (fun x -> e)
      | xc xc
      | if xc then e else e
      | match xc with c -> e | ... | _ -> e end
      | match xc with inj p -> e | ... | {_ -> e}? end
      | let p = e in e
      | reg_l (fun p -> e) last xc
      | exec_l e xc
      | var x = e in e
      | x <- xs
      | x[xc]
      | x.length
      | x[xc] <- xc

  xc := (xc,xc ... xc)
      | c
      | x
      | fst x | snd x | ...
*)

let rec name_kont_in e k =
  match e with
  | E_var _  -> k e
  | E_letIn(p,e1,e2) ->
      E_letIn(p,e1,name_kont_in e2 k)
  | E_if(e1,e2,e3) ->
      E_if(e1,name_kont_in e2 k,name_kont_in e3 k)
  | E_case(e,hs,e_els) ->
      E_case(e,
             List.map (fun (c,e) -> c,name_kont_in e k) hs,
             name_kont_in e_els k)
  | E_match(e,hs,eo) ->
      E_match(e,
              List.map (fun (x,(p,e)) -> x,(p,name_kont_in e k)) hs,
              Option.map (fun e -> name_kont_in e k) eo)
  | _ -> assert false

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
    (* | E_var x::es' -> (* ok?*)plug_n_aux aas es' context *)
    | e::es' -> plug e @@ fun v -> plug_n_aux (v::aas) es' context
  in
  plug_n_aux [] es context

(** [anf e] puts expression [e] in ANF-form *)
let rec anf e =
  let _,e' = anf_level e in e'
and anf_level (e:e) =
  match e with
  | E_deco _ ->
      Ast_undecorated.still_decorated e
  | E_var _ | E_const _ -> 1,e
  | E_fun(x,e1) ->
      1,E_fun(x,anf e1)
  | E_fix(f,(x,e1)) ->
      1,E_fix(f,(x,anf e1))
  | E_if(e1,e2,e3) ->
      1,
      plug (anf e1) @@ fun xc ->
      E_if(xc,anf e2,anf e3)
  | E_case(e,hs,e_els) ->
      1,plug (anf e) @@ fun xc ->
      E_case(xc,List.map (fun (c,e) -> c,anf e) hs,anf e_els)
  | E_match(e,hs,eo) ->
      1,plug (anf e) @@ fun xc ->
      E_match(xc,List.map (fun (x,(p,e)) -> x,(p,anf e)) hs,Option.map anf eo)
  | E_letIn(P_var f,(E_fix(g,(p,e1))),e2) when f <> g ->
      assert (not (pat_mem f p) && not (pat_mem g p));
      1,anf @@ E_letIn(P_var f,E_fix(f,(p,subst_e g (E_var f) e1)),e2)
  | E_letIn(P_var x,(E_var _ as e1),e2) ->
      1,anf @@ subst_e x e1 e2
  | E_letIn(p,e1,e2) ->
      1,E_letIn(p,anf e1,anf e2)
  | E_tuple(es) ->
      List.length es,
      plug_n (List.map anf es) @@
      fun xs -> E_tuple(xs)
  | E_app(E_const _ as ec,e2) ->
      1,plug (anf e2) @@ fun x2 ->
      E_app(ec,x2)
  | E_app(e1,e2) ->
      1,plug (anf e1) @@ fun xc1 ->
      let n,e2' = anf_level e2 in
      let xs = List.init n (fun x -> gensym ()) in(* plug (anf e2) @@ fun xc2 -> *)
      E_letIn(group_ps (List.map (fun x -> P_var x) xs),e2',
      E_app(xc1,group_es (List.map (fun x -> E_var x) xs)))
  | E_reg((p,e1),e0,l) ->
      1,plug (anf e0) @@ fun xc0 ->
      E_reg((p,anf e1),xc0,l)
    | E_exec(e1,e0,l) ->
        1,plug (anf e0) @@ fun xc0 ->
        E_exec(anf e1,xc0,l)
  | E_ref(e1) ->
      1,plug (anf e1) @@ fun xc1 ->
        E_ref(xc1)
  | E_get(e1) ->
      1,name_kont_in e1 (fun x -> E_get x)
  | E_set(e1,e2) ->
      1,name_kont_in e1 (fun x -> plug (anf e2) @@ 
                           fun xc1 -> E_set(x,xc1))
  | E_array_length _ ->
      1,e
  | E_array_get(x,e1) ->
      1,plug (anf e1) @@ fun xc1 ->
      E_array_get(x,xc1)
  | E_array_set(x,e1,e2) ->
      1,plug (anf e1) @@ fun xc1 ->
      plug (anf e2) @@ fun xc2 ->
      E_array_set(x,xc1,xc2)
  | E_matrix_size _ ->
      1,e
  | E_matrix_get(x,es) ->
      1,plug_n (List.map anf es) @@ fun xs ->
      E_matrix_get(x,xs)
  | E_matrix_set(x,es,e2) ->
      1,plug_n (List.map anf es) @@ fun xs -> 
      plug (anf e2) @@ fun xc2 ->
      E_matrix_set(x,xs,xc2)
  | E_par(es) ->
      1,E_par(List.map anf es)
  | E_absLabel(l,e1) ->
      1,E_absLabel(l,anf e1)
  | E_appLabel(e1,l,lc) ->
      1,E_appLabel(anf e1,l,lc)
  | E_for(x,e_st1,e_st2,e3,loc) ->
      1,E_for(x,anf e_st1,anf e_st2,anf e3,loc)
      (* NB: [e_st1] and [e_st2] are *not* moved up with `plug` *)
  | E_generate((p,e1),e2,e_st3,loc) ->
      1,plug (anf e2) @@ fun xc ->
      E_generate((p,anf e1),xc,anf e_st3,loc) 
      (* NB: [e_st3] is *not* moved up with `plug` *)

(** [in_anf e] check if expression [e] is in ANF-form *)
let rec in_anf (e:e) : bool =
  match e with
  | E_deco(e1,_) ->
      in_anf e1
  | E_var _ | E_const _ -> true
  | E_fun(_,e1) ->
      in_anf e1
  | E_fix(_,(_,e1)) ->
      in_anf e1
  | E_if(e1,e2,e3) ->
      is_xc e1 && in_anf e2 && in_anf e3
  | E_case(e1,hs,e_els) ->
      is_xc e1 && List.for_all (fun (_,e) -> in_anf e) hs && in_anf e_els
| E_match(e1,hs,eo) ->
      is_xc e1 && List.for_all (fun (_,(_,e)) -> in_anf e) hs &&
      (match eo with
       | None -> true
       | Some e -> in_anf e)
  | E_letIn(_,e1,e2) ->
      in_anf e1 && in_anf e2
  | E_tuple(es) ->
      List.for_all is_xc es
  | E_app(e1,e2) ->
      is_xc e1 && is_xc e2
  | E_reg((_,e1),e0,_) ->
      in_anf e1 && is_xc e0
  | E_exec(e1,e0,l) ->
       in_anf e1 && is_xc e0
  | E_ref(e1) -> is_xc e1
  | E_get _ -> true
  | E_set(x,e1) ->
      is_xc e1
  | E_array_length _ ->
      true
  | E_array_get(x,e1) ->
      is_xc e1
  | E_array_set(x,e1,e2) ->
      is_xc e1 && is_xc e2
  | E_matrix_size _ ->
      true
  | E_matrix_get(x,es) ->
      List.for_all is_xc es
  | E_matrix_set(x,es,e2) ->
      List.for_all is_xc es && is_xc e2 
  | E_par(es) ->
      List.for_all in_anf es
  | E_absLabel(_,e1) ->
      in_anf e1
  | E_appLabel(e1,_,_) ->
      in_anf e1
  | E_for(_,e_st1,e_st2,e3,_) ->
      in_anf e_st1 && in_anf e_st2 && in_anf e3
  | E_generate((_,e1),e2,e_st3,_) ->
      in_anf e1 && is_xc e2 && in_anf e_st3

(** [anf e] puts program [pi] in ANF-form *)
let anf_pi (pi:pi) : pi =
  { pi with main = anf pi.main }


(** [in_anf_pi pi] check if program [pi] is in ANF-form *)
let in_anf_pi (pi:pi) : bool =
  in_anf pi.main
