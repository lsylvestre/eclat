open Ast
open Ast_subst

open Pattern

let declare ds e =
  List.fold_right (fun (p,t,v) e -> E_letIn(p,t,v,e)) ds e


let rec lfloat (e:e) : e =
  let rec glob (e:e) : _ * e =
  match e with
  | E_deco _ ->
      Ast_undecorated.still_decorated e
  | E_var _ | E_const _ -> [],e
  | E_fun(p,ty,e1) ->
      (** does not move up let-bindings outside function definition
          since they have to be reevaluated at each function call *)
      [],E_fun(p,ty,lfloat e1)
  | E_fix(f,(p,ty,e1)) ->
      (** does not move up let-bindings outside function definition
          since they have to be reevaluated at each function call *)
      [],E_fix(f,(p,ty,lfloat e1))
  | E_if(e1,e2,e3) ->
      (** does not move up let-bindings outside the then branch 
          (resp. the else branch) since they have to be evaluated 
          only when the condition is true *)
      let ds1,e1' = glob e1 in
      ds1,E_if(e1',lfloat e2,lfloat e3)
  | E_case(e1,hs,e_els) ->
      (** does not move up let-bindings outside each case [hs/e_els] 
          since they have to be evaluated only when [e1] matches them *)
      let ds1,e1' = glob e1 in
      ds1, E_case(e1' ,List.map (fun (c,e) -> c,lfloat e) hs,lfloat e_els)
  | E_match(e1,hs,eo) ->
      (** does not move up let-bindings outside each case [hs/eo] 
          since they have to be evaluated only when [e1] matches them *)
      let ds1,e1' = glob e1 in
      ds1,E_match(e1',List.map (fun (x,(p,e)) -> x,(p,lfloat e)) hs,Option.map lfloat eo)
  | E_letIn(P_var x, ty,E_var y, e) ->
      (* copy/propagation (just for optimisation) *)
      glob @@ (subst_e x (E_var y) e)
  | E_letIn(P_tuple ps,ty,E_tuple es,e2) ->
      let ts = match Types.canon_ty ty with 
               | Ty_tuple ts -> ts 
               | (* Ty_var*) _ -> List.map (fun _ -> Types.new_ty_unknown()) ps 
               (* | _ -> assert false *)in
      glob (List.fold_right2 (fun (p,t) e acc -> E_letIn(p,t,e,acc)) (List.combine ps ts) es e2)
  | E_letIn(p,ty,e1,e2) ->
      let ds1,e1' = glob e1 in
      let e' = lfloat e2 in
      ds1@[(p,ty,e1')],e'
  | E_tuple(es) ->
      let dss,es' = List.split (List.map glob es) in
      List.concat dss,E_tuple es'
  | E_app(E_const _ as ec,e2) ->
      let ds2,e2' = glob e2 in
      ds2,E_app(ec,e2')
  | E_app(e1,e2) ->
      let ds1,e1' = glob e1 in
      let ds2,e2' = glob e2 in
      ds1@ds2,E_app(e1',e2')
  | E_reg((p,tyB,e1),e0,l) ->
      let ds0,e0' = glob e0 in
      ds0,E_reg((p,tyB,lfloat e1),e0',l)
  | E_exec(e1,e0,eo,l) ->
      let ds0,e0' = glob e0 in
      let ds3,eo' = match eo with 
                    | None -> [],eo 
                    | Some e3 -> let ds3,e3' = glob e3 in
                    ds3,Some e3 in
      ds0@ds3,E_exec(lfloat e1,e0',eo',l)
  | E_ref(e1) ->
      let ds1,e1' = glob e1 in
      ds1,E_ref(e1')
  | E_get(e1) ->
      let ds1,e1' = glob e1 in
      ds1,E_get(e1')
  | E_set(e1,e2) ->
      let ds1,e1' = glob e1 in
      let ds2,e2' = glob e2 in
      ds1@ds2, E_set(e1',e2')
  | E_array_length _ ->
      [],e
  | E_array_make(sz,e1,loc) ->
      let ds1,e1' = glob e1 in
      ds1,E_array_make(sz,e1',loc)
  | E_array_create _ ->
      [],e
  | E_array_get(x,e1) ->
      let ds1,e1' = glob e1 in
      ds1,E_array_get(x,e1')
  | E_array_set(x,e1,e2) ->
      let ds1,e1' = glob e1 in
      let ds2,e2' = glob e2 in
      ds1@ds2,E_array_set(x,e1',e2')
  | E_par(es) ->
      [],E_par(List.map lfloat es)
  | E_for(x,e_st1,e_st2,e3,loc) ->
      [],E_for(x,lfloat e_st1,lfloat e_st2,lfloat e3,loc)
      (* NB: [e_st1] and [e_st2] are *not* moved up with `plug` (ah ? from anf)  *)
  | E_generate((p,ty,e1),e2,e_st3,loc) ->
      let ds2,e2' = glob e2 in
      ds2,E_generate((p,ty,lfloat e1),e2',lfloat e_st3,loc) 
      (* NB: [e_st3] is *not* moved up with `plug` (ah ? from anf) 
*)
  | E_vector(es) ->
      let dss,es' = List.split (List.map glob es) in
      List.concat dss,E_vector es'
  | E_vector_mapi(is_par,(p,typ,e1),e2,ty) ->
      let ds2,e2' = glob e2 in
      ds2,E_vector_mapi(is_par,(p,typ,lfloat e1),e2',ty) 
  | E_run(i,e) ->
      let ds,e' = glob e in
      ds,E_run(i,e)
  | E_pause e -> 
      let ds,e' = glob e in
      ds,E_pause e'
  in 
  let ds,e' = glob e in 
  declare ds e'


(* move up let-bindings as far as possible *)
let float_pi (pi:pi) : pi =
  { pi with main = lfloat pi.main }

