open Ast
open Ast_subst


(**

goal: given a program in ANF-form, e.g.
  [let (f,g) = let f1 = fun x -> e in
               let f2 = fun y -> e' in
               (f1,f2)
   in
   (f(g(3)))],

systematically moves up bindings to obtain:

     [let f1 = fun x -> e in
      let f2 = fun y -> e' in
      let (f,g) = (f1,f2) in
      (f(g(3)))]

    which is needed (after copy propagation) for eliminating high-order
    (ensuring in particular, above, that tuples do not contain functions. *)

(* [!!] must avoid scope extrusion and preserve the order of computations *)


(** [let_floating e] perform let floating on expression [e] *)
let rec let_floating (e:e) : e =
  let bs, e = glob e in
  List.fold_right (fun (p,e) acc -> E_letIn(p,e,acc)) bs e

and glob (e:e) : ((p * e) list * e) =
  let open Ast in
  match e with
  | E_deco _ ->
      Ast_undecorated.still_decorated e
  | E_const _ | E_var _ ->
      [],e
    | E_app(e1,e2) ->
      let ds1,e1' = glob e1 in
      let ds2,e2' = glob e2 in
      ds1@ds2,E_app(e1',e2')
  | E_tuple(es) ->
      let dss,es' = List.map glob es |> List.split in
      List.concat dss,E_tuple(es')
  | E_fix(f,(x,e)) ->
      [],E_fix(f,(x,let_floating e))
  | E_fun(x,e) ->
      [],E_fun(x,let_floating e)
  | E_if(e1,e2,e3) ->
      let ds1,e1' = glob e1 in
      ds1,E_if(e1',let_floating e2,let_floating e3)
  | E_case(e1,hs,e_els) ->
      let ds1,e1' = glob e1 in
      ds1,E_case(e1',List.map (fun (c,e) -> c,let_floating e) hs,let_floating e_els)
  | E_match(e1,hs,eo) ->
      let ds1,e1' = glob e1 in
      let hs' = List.map (fun (x,(p,e)) -> x,(p,let_floating e)) hs in
      let dsw,eo' = match eo with
                    | None -> [],eo
                    | Some ew -> let dsw,ew' = glob ew in
                                 (dsw,Some ew')
      in
      dsw@ds1,E_match(e1',hs',eo')
  | E_letIn(p,e1,e2) ->
      let ds1,e1' = glob e1 in
      ds1@[(p,e1')],let_floating e2
  | E_reg((p,e1),e0,l) ->
      let ds0,e0' = glob e0 in
      ds0,E_reg((p,let_floating e1),e0',l)
  | E_exec(e1,e0,l) ->
      let ds0,e0' = glob e0 in
      ds0,E_exec(let_floating e1,e0',l)
  | E_lastIn(x,e1,e2) ->
      [],E_lastIn(x,let_floating e1,let_floating e2)
  | E_set(x,e1) ->
      let ds1,e1' = glob e1 in
      ds1,E_set(x,e1')
  | E_static_array_get(x,e1) ->
      let ds1,e1' = glob e1 in
      ds1,E_static_array_get(x,e1')
  | E_static_array_length _ ->
      [],e
  | E_static_array_set(x,e1,e2) ->
      let ds1,e1' = glob e1 in
      let ds2,e2' = glob e2 in
      ds1@ds2,E_static_array_set(x,e1',e2')
  | E_par(e1,e2) ->
      [],E_par(let_floating e1,let_floating e2)


(** [let_floating_pi pi] perform let floating on program [pi] *)
let let_floating_pi pi =
  Map_pi.map let_floating pi
