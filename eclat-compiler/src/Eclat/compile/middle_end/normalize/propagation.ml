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
    (*| E_app(E_const(Op(GetTuple{pos;arity})),e1) -> simple_atom e1*)
    (* | E_app(E_const(Op(op)),e1) -> Combinational.op_combinational op  && simple_atom e1 *)
    | _ -> false


let propagation ~genv e =
  let _propagable e =
    if !flag_propagate_combinational_linear then Instantaneous.combinational ~with_sig_get:false ~externals:genv.operators e 
                                  && (SMap.cardinal (linear_bindings e) <= 1) else
    simple_atom e
  in
  let _propagable2 x e1 e2 =
    let b = if !flag_propagate_combinational_linear then
      (Instantaneous.combinational ~with_sig_get:false ~externals:genv.operators e1
      && linear_bindings2 x e2) || simple_atom e1 else
    simple_atom e1 in (*Printf.printf "=====> %s %b\n" x b;*) b
  in
  let rec prop e =
    (* let e' =*) match e with
    | E_letIn(p,ty,E_sig_create a,e2) ->
      E_letIn(p,ty,E_sig_create a,prop e2)
    | E_letIn(p,ty,E_trap a,e2) ->
      E_letIn(p,ty,E_trap a,prop e2)
    | E_letIn(P_tuple ps,ty,E_tuple es,e2) ->
        let ts = match Types.canon_ty ty with 
                 | Ty_tuple ts -> ts
                 | Ty_base (TyB_tuple tyBs) -> List.map (fun tyB ->Types.Ty_base tyB) tyBs
                 | _ -> List.map (fun _ -> Types.new_ty_unknown()) ps in
        prop @@ List.fold_right2 (fun (pi,ti) ei e -> E_letIn(pi,ti,ei,e)) 
                  (List.combine ps ts) es e2
    | E_letIn(P_var x as p,ty,e1,e2) ->
        let e1' = prop e1 in
        if simple_atom e1' (* _propagable2 x e1' e2 *)
              (* _propagable2 is true if x occurs only once in e2:
                 the problem is that the substitution of non atomic expressions
                 can result in a loss of type annotations (especially for sizes) *)
        then let e3 = subst_e ~when_var:Inline.subst_ty x e1' e2 in
        (*Format.(fprintf std_formatter "[%a\nGIVE:LET %s = %a IN\n%a]\n\n\n\n\n\n\n" Ast_pprint.pp_exp e x Ast_pprint.pp_exp e1' Ast_pprint.pp_exp e3);*)
             (prop e3)
        else E_letIn(p,ty,e1',prop e2)
    | E_letIn(p,ty,e1,e2) ->
        E_letIn(p,ty,prop e1,prop e2)
    | E_app(E_const(Op(GetTuple{pos;arity})),E_tuple vs) ->
        prop (List.nth vs pos)
    | e -> Ast_mapper.map prop e
    in (*(Format.(fprintf std_formatter "[<==================\n%a---->\n%a==============>]\n\n\n\n\n\n\n\n\n\n\n" Ast_pprint.pp_exp e Ast_pprint.pp_exp e'); e')*)
   prop e 

let propagation_pi pi =
  {pi with main = propagation ~genv:pi.genv pi.main }
