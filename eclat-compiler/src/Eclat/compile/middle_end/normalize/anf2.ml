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
let rec is_var (e:e) : bool =
  match un_deco e with
  | E_var _ -> true | _ -> false

let rec is_xc (e:e) : bool =
  match un_deco e with
  | E_deco(e,_) -> is_xc e
  | E_var _ | E_const _ -> true
  | E_tuple(es) -> List.for_all is_xc es
  | E_app(E_const(Op(Runtime(op))),e) -> 
     (try Operators.combinational ~externals:("",[]) op && is_xc e 
     with _ -> false)
  | E_app(E_const(Op _),e) -> is_xc e
  | E_array_length _ -> true
  | _ -> false

(** [plug e ctx] plugs expression [e] into context [ctx], i.e., returns
   [ctx e] if [criterion e] is true, otherwise [let y = e in ctx y]
   with [y] a fresh name. *)
let plug ?(criterion=is_xc) (e:e) (context: e -> e) : e =
  if criterion e then context e else
  let x = gensym () in
  E_letIn(P_var x, Types.new_ty_unknown(), e,context (E_var x))

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
  let rec glob (e:e) : e =
  match e with
  | E_deco _ ->
      Ast_undecorated.still_decorated e
  | E_var _ | E_const _ -> e
  | E_fun(p,ty,e1) ->
      E_fun(p,ty,glob e1)
  | E_fix(f,(p,ty,e1)) ->
      E_fix(f,(p,ty,glob e1))
  | E_if(e1,e2,e3) ->
      plug ~criterion:is_var (glob e1) @@ fun x1 ->
      E_if(x1,glob e2, glob e3)
  | E_case(e1,hs,e_els) ->
      plug ~criterion:is_var (glob e1) @@ fun xc1 ->
      E_case(xc1,List.map (fun (c,e) -> c,glob e) hs, glob e_els)
  | E_match(e1,hs,eo) ->
       plug ~criterion:is_var (glob e1) @@ fun xc1 ->
      E_match(xc1,List.map (fun (x,(p,e)) -> x,(p,glob e)) hs,Option.map glob eo)
  | E_letIn(p,ty,e1,e2) ->
       E_letIn(p,ty,glob e1,glob e2)
  | E_tuple(es) ->
      plug_n (List.map glob es) @@ fun xs -> E_tuple(xs)
  | E_app(E_const(Op(Runtime(Get_tuple _))) as e1,e2) ->
     plug ~criterion:is_var (glob e2) @@ fun x2 -> E_app(e1,x2)
  | E_app(E_const(Op(Runtime(Start_read _ | Start_write _))) as e1,e2) ->
        (match e2 with
        | E_tuple es -> 
           plug_n (List.map glob es) @@ fun xs ->
              E_app(e1,E_tuple xs)
        | _ -> assert false)
  | E_app(E_const(Op(Runtime(_))) as e1,e2) ->
        (match e2 with
        | E_tuple es -> 
           plug_n (List.map glob es) @@ fun xs ->
              E_app(e1,E_tuple xs)
        | _ -> plug (glob e2) @@ fun xc2 ->
                  E_app(e1,xc2))
  | E_app(ec,e2) ->
      plug (glob e2) @@ fun xc2 ->
      E_app(ec,xc2)

  (* | (E_app _ as ee) ->  (* assume argument is already atomic *) 
      ee*)
  | E_reg((p,tyB,e1),e0,l) ->
      E_reg((p,tyB,glob e1),glob e0,l)
  | E_exec _ 
  | E_ref _
  | E_get _ 
  | E_set _ -> e
  | E_array_length _ ->
      e
  | E_array_make(sz,e1,loc) ->
      let e1' = glob e1 in
      E_array_make(sz,e1',loc)
  | E_array_create _ ->
      e
  | E_array_get(x,e1) ->
      let e1' = glob e1 in
      plug e1' @@ fun xc1 ->
          E_array_get(x,xc1)
  | E_array_set(x,e1,e2) ->
      let e1' = glob e1 in
      let e2' = glob e2 in
      plug e1' @@ fun xc1 ->
      plug e2' @@ fun xc2 ->
      E_array_set(x,xc1,xc2)
  | E_par(es) ->
      E_par(List.map anf es)
  (*| E_absLabel(l,e1) ->
      E_absLabel(l,anf e1)
  | E_appLabel(e1,l,lc) ->
      E_appLabel(anf e1,l,lc)*)
  | E_for(x,e_st1,e_st2,e3,loc) ->
      E_for(x,anf e_st1,anf e_st2,anf e3,loc)
      (* NB: [e_st1] and [e_st2] are *not* moved up with `plug` *)
  | E_generate((p,ty,e1),e2,e_st3,loc) ->
      plug (anf e2) @@ fun xc ->
      E_generate((p,ty,anf e1),xc,anf e_st3,loc) 
      (* NB: [e_st3] is *not* moved up with `plug` 
*)
  | E_vector(es) ->
      let es' = List.map glob es in
      plug_n es' @@ fun xs -> E_vector(xs)
  | E_vector_mapi(is_par,(p,typ,e1),e2,ty) ->
      plug (anf e2) @@ fun xc ->
      E_vector_mapi(is_par,(p,typ,anf e1),xc,ty) 
  | E_run(i,e,l) ->
      plug (anf e) @@ fun xc ->
      E_run(i,xc,l) 
  | E_pause e -> E_pause (glob e)
  | E_equations(p,eqs) ->
      let rec anf_le le =
        match le with
        | Exp e1 -> Exp (anf e1)
        | Fby(le1, le2) -> Fby(anf_le le1, anf_le le2)
        | When(le1, e2) -> When(anf_le le1, anf e2)
        | Merge(le1, le2, e3) -> Merge(anf_le le1, anf_le le2, anf e3)
      in
      E_equations(p,List.map (fun (p,le) -> p, anf_le le) eqs)
  in 
  glob e ;;

(** [anf e] puts program [pi] in ANF-form *)
let anf_pi (pi:pi) : pi =
  { pi with main = anf pi.main }

