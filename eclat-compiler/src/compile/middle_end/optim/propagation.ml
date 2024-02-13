open Ast
open Ast_subst

 (** propagate combinational expressions bound to a name used zero or one time *)
let flag_propagate_combinational_linear = ref true


(* [linear_bindings e] produces the set of the names that are locally defined in [e]
  occurring (as variable) exactly once *)
let linear_bindings (e:e) : set =
  let h = Hashtbl.create 10 in
  let rec aux = function
  | E_deco(e,_) ->
      aux e
  | E_var x ->
      (match Hashtbl.find_opt h x with
      | None -> Hashtbl.add h x true
      | Some v -> Hashtbl.replace h x false)
  | E_const _ ->
      ()
  | E_if(e1,e2,e3) ->
      aux e1; aux e2; aux e3
  | E_case(e1,hs,e_els) ->
      aux e1; List.iter (fun (_,ei) -> aux ei) hs; aux e_els
  | E_match(e1,hs,eo) ->
      aux e1; List.iter (fun (_,(_,ei)) -> aux ei) hs;
      Option.iter aux eo
  | E_letIn(p,e1,e2) ->
      aux e1; aux e2
  | E_app(e1,e2) ->
      aux e1; aux e2
  | E_fun(p,e1) ->
      aux e1
  | E_fix(f,(p,e)) ->
      aux e
  | E_tuple(es) ->
      List.iter aux es
  | E_reg((_,e1), e0, _) ->
      aux e1; aux e0
  | E_exec(e1,e2,_k) ->
      aux e1; aux e2
   | E_lastIn(x,e1,e2) ->
      aux e1; aux e2
  | E_set(x,e1) ->
      aux e1
  | E_static_array_get(x,e1) ->
      aux e1
  | E_static_array_length(x) ->
      ()
  | E_static_array_set(x,e1,e2) ->
      aux e1; aux e2
  | E_par(e1,e2) ->
      aux e1; aux e2
  in
  aux e;
  let keep x b acc =
    if b then SMap.add x () acc else acc
  in
  Hashtbl.fold keep h SMap.empty



let rec simple_atom e =
  match e with
    | E_var _ | E_const _ -> true
    | E_tuple es -> List.for_all simple_atom es
    | _ -> false


let propagation e =
  let propagable e =
    if !flag_propagate_combinational_linear then Combinational.combinational e else
    simple_atom e in
  let rec prop e =
    match e with
    | E_letIn(P_tuple ps,E_tuple es,e2) ->
        List.fold_left2 (fun e pi ei -> subst_p_e pi (prop ei) e) (prop e2) ps es
    | E_letIn(P_var x as p,e1,e2) ->
        let e1' = prop e1 in
        if propagable e1'
        then prop (subst_p_e p e1' e2)
        else E_letIn(p,e1',prop e2)
    | E_letIn(p,e1,e2) ->
        E_letIn(p,prop e1,prop e2)
    | E_app(E_const(Op(GetTuple{pos;arity})),E_tuple vs) ->
        prop (List.nth vs pos)
    | E_app(e1,e2) ->
        E_app(prop e1,prop e2)
    | E_static_array_set(x,e1,e2) ->
        E_static_array_set(x,prop e1,prop e2)
    | e -> Ast_mapper.map prop e
  in prop e

let propagation_pi pi =
  {pi with main = propagation pi.main }
