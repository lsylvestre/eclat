open Ast

(** traversal order of sub-expressions is unspecified *)

let rec map f e =
  match e with
  | E_deco(e,ty) ->
      E_deco(f e,ty)
  | E_const _ | E_var _ -> e
  | E_fun(p,e) ->
      E_fun(p,f e)
  | E_fix(x,(p,e)) ->
      E_fix(x,(p,f e))
  | E_tuple es ->
      E_tuple (List.map f es)
  | E_app(e1,e2) ->
      E_app(f e1,f e2)
  | E_if(e1,e2,e3) ->
      E_if(f e1,f e2,f e3)
  | E_case(e1,hs,e_els) ->
      E_case(f e1,List.map (fun (c,e) -> c,f e) hs,f e_els)
  | E_match(e1,hs,eo) ->
      E_match(f e1,List.map (fun (c,(p,e)) -> c,(p,f e)) hs,Option.map f eo)
  | E_letIn(p,e1,e2) ->
     E_letIn(p,f e1,f e2)
  | E_lastIn(x,e1,e2) ->
      E_lastIn(x,f e1,f e2)
  | E_set(x,e1) ->
      E_set(x,f e1)
  | E_static_array_get(x,e1) ->
      E_static_array_get(x,f e1)
  | E_static_array_length _ ->
      e
  | E_static_array_set(x,e1,e2) ->
      E_static_array_set(x,f e1, f e2)
  | E_par(e1,e2) ->
      E_par(f e1,f e2)
  | E_reg((p,e1),e0,l) ->
      E_reg((p,f e1),f e0,l)
  | E_exec(e1,e2,k) ->
      E_exec(f e1,f e2,k)
  | E_absLabel(l,e1) ->
      E_absLabel(l, f e1)
  | E_appLabel(e1,l) ->
      E_appLabel(f e1,l)

(** traversal order of sub-expressions is unspecified *)

let rec iter f (e:e) : unit =
  match e with
  | E_deco (e,_) ->
      f e
  | E_var _ ->
      ()
  | E_const c ->
      ()
  | E_if(e1,e2,e3) ->
      f e1; f e2; f e3
  | E_case(e1,hs,e_els) ->
      f e1; List.iter (fun (_,ei) -> f ei) hs; f e_els
  | E_match(e1,hs,eo) ->
      f e1; List.iter (fun (_,(_,ei)) -> f ei) hs; Option.iter f eo
  | E_app(e1,e2) ->
      f e1; f e2
  | E_letIn(_,e1,e2) ->
      f e1; f e2
  | E_tuple es ->
      List.iter f es
  | E_fun(_,e) | E_fix(_,(_,e)) ->
      f e
  | E_lastIn(_,e1,e2) ->
      f e1; f e2
  | E_set(_,e1) ->
      f e1
  | E_par(e1,e2) ->
      f e1; f e2
   | E_reg((_,e1),e0,_) ->
      f e1; f e0
  | E_exec(e1,e2,_) ->
      f e1; f e2
  | E_static_array_length _ ->
      ()
  | E_static_array_get(_,e1) ->
      f e1
  | E_static_array_set(_,e1,e2) ->
      f e1; f e2
  | E_absLabel(_,e1) ->
      f e1
  | E_appLabel(e1,_) ->
      f e1


let map_pi f pi =
  Map_pi.map (map f) pi
