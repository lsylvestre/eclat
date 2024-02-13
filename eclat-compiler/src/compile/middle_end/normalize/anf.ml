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

(** [anf e] puts expression [e] in ANF-form *)
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
  | E_lastIn(x,e1,e2) ->
      E_lastIn(x,anf e1,anf e2)
  | E_set(x,e1) ->
      plug (anf e1) @@ fun xc1 ->
      E_set(x,xc1)
  | E_static_array_get(x,e1) ->
      plug (anf e1) @@ fun xc1 ->
      E_static_array_get(x,xc1)
  | E_static_array_length _ ->
      e
  | E_static_array_set(x,e1,e2) ->
      plug (anf e1) @@ fun xc1 ->
      plug (anf e2) @@ fun xc2 ->
      E_static_array_set(x,xc1,xc2)
  | E_par(e1,e2) ->
      E_par(anf e1, anf e2)


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
  | E_lastIn(x,e1,e2) ->
      in_anf e1 && in_anf e2
  | E_set(x,e1) ->
      is_xc e1
  | E_static_array_get(x,e1) ->
      is_xc e1
  | E_static_array_length _ ->
      true
  | E_static_array_set(x,e1,e2) ->
      is_xc e1 && is_xc e2
  | E_par(e1,e2) ->
      in_anf e1 && in_anf e2


(** [anf e] puts program [pi] in ANF-form *)
let anf_pi (pi:pi) : pi =
  { pi with main = anf pi.main }


(** [in_anf_pi pi] check if program [pi] is in ANF-form *)
let in_anf_pi (pi:pi) : bool =
  in_anf pi.main
