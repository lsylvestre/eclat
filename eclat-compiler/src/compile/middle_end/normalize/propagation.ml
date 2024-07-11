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
  | _ -> Ast_mapper.iter aux e
  in
  aux e;
  let keep x b acc =
    if b then SMap.add x () acc else acc
  in
  Hashtbl.fold keep h SMap.empty


let linear_bindings2 x (e:e) : bool =
  let r = ref 0 in
  let exception Found in
  let rec aux e = match e with
  | E_deco(e,_) ->
      aux e
  | E_var y ->  
      if x = y then ((if !r > 0 then (raise Found)); incr r)
  | E_const _ ->
      ()
  | _ -> Ast_mapper.iter aux e
  in
  try aux e; true with Found -> false


let rec simple_atom e =
  match e with
    | E_var _ | E_const _ -> true
    | E_tuple es -> List.for_all simple_atom es
    (* | E_app(E_const(Op(op)),e1) -> Combinational.op_combinational op  && simple_atom e1 *)
    | _ -> false


let propagation e =
  let _propagable e =
    if !flag_propagate_combinational_linear then Instantaneous.combinational e 
                                  && (SMap.cardinal (linear_bindings e) <= 1) else
    simple_atom e 
  in
  let propagable2 x e =
    if !flag_propagate_combinational_linear then Instantaneous.combinational e 
                                  && linear_bindings2 x e else
    simple_atom e 
  in
  let rec prop e =
    match e with
    | E_letIn(P_tuple ps,E_tuple es,e2) ->
        prop @@ List.fold_right2 (fun pi ei e -> E_letIn(pi,ei,e)) ps es e2
    | E_letIn(P_var x as p,e1,e2) ->
        let e1' = prop e1 in
        if propagable2 x e1'
        then prop (subst_p_e p e1' e2)
        else E_letIn(p,e1',prop e2)
    | E_letIn(p,e1,e2) ->
        E_letIn(p,prop e1,prop e2)
    | E_app(E_const(Op(GetTuple{pos;arity})),E_tuple vs) ->
        prop (List.nth vs pos)
    | E_app(e1,e2) ->
        E_app(prop e1,prop e2)
    | e -> Ast_mapper.map prop e
  in prop e

let propagation_pi pi =
  {pi with main = propagation pi.main }
