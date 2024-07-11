(* Heuristic to determine if a given expression is instantaneous *)

open Ast

let op_combinational (op:op) : bool =
	match op with
	| Runtime(p) ->
	    Operators.combinational p
	| GetTuple _ -> true
	| TyConstr _ -> true
	| _ -> true


let const_combinational (c:c) : bool =
	match c with
  | _ -> true

let rec combinational (e:e) : bool =
  match e with
	| E_deco(e1,loc) ->
      combinational e1
	| E_var _ ->
	    true
	| E_const c ->
	    const_combinational c
	| E_if(e1,e2,e3) ->
	    combinational e1 && combinational e2 && combinational e3
	| E_case _ | E_match _ ->
      (* hard to defined as a combinational hardware description being generic in the number of cases *)
      false
	| E_app(e1,e2) ->
      (match un_deco e1 with
      | E_const(Inj _) -> combinational e2
      | E_const(Op op) ->
          op_combinational op && combinational e2
      | _ ->
         (* as we can't know locally whether [e1] is combinational,
	       we return false *)
	       false)
	| E_letIn(_,e1,e2) ->
	    combinational e1 && combinational e2
	| E_tuple es ->
	    List.for_all combinational es
	| E_fun _ | E_fix _ ->
	    false
  | E_ref _ | E_get _ | E_set _ | E_par _ ->
	    false
	| E_reg _ | E_exec _ ->
	    false (* have an internal state *)
  | E_array_length _ ->
	    true
	| E_local_static_array _ ->
      false
	| E_array_get _
	| E_array_set _ ->
	    false (* side effect *)
	| E_for(_,e_st1,e_st2,e3,_) ->
	   combinational e_st1 && combinational e_st2 && combinational e3
	| E_generate((_,e1),e2,e_st3,_) ->
	    combinational e1 && combinational e2 && combinational e_st3
	| E_vector es -> List.for_all combinational es
	| E_vector_mapi(is_par,(_,e1),e2,_) ->
	    not(is_par) && combinational e1 && combinational e2
	| E_int_mapi(is_par,(_,e1),e2,_) ->
	    not(is_par) && combinational e1 && combinational e2



(* same as combinational, but may contain cas/match, registers or even exec blocks *)

let rec instantaneous (e:e) : bool =
  match e with
	| E_deco(e1,loc) ->
      instantaneous e1
	| E_var _ ->
	    true
	| E_const c ->
	    const_combinational c
	| E_if(e1,e2,e3) ->
	    instantaneous e1 && instantaneous e2 && instantaneous e3
  | E_case(e1,hs,eothers) ->
	    instantaneous e1 && List.for_all (fun (_,e) -> instantaneous e) hs && instantaneous eothers
  | E_match(e1,hs,eo) ->
      instantaneous e1 && List.for_all (fun (c,(p,e)) -> instantaneous e) hs && 
      (match eo with None -> true | Some e2 -> instantaneous e2)
	| E_app(e1,e2) ->
      (match un_deco e1 with
      | E_const(Inj _) -> instantaneous e2
      | E_const(Op op) ->
          op_combinational op && instantaneous e2
      | _ ->
         (* as we can't know locally whether [e1] is instantaneous,
	       we return false *)
	       false)
	| E_letIn(_,e1,e2) ->
	    instantaneous e1 && instantaneous e2
	| E_tuple es ->
	    List.for_all instantaneous es
	| E_fun _ | E_fix _ ->
	    false
  | E_ref _ | E_get _ | E_set _ | E_par _ ->
	    false
	| E_reg _ | E_exec _ ->
	    true (* have an internal state *)
  | E_array_length _ ->
	    true
	| E_local_static_array _ ->
      false
	| E_array_get _
	| E_array_set _ ->
	    false (* side effect *)
	| E_for(_,e_st1,e_st2,e3,_) ->
	   instantaneous e_st1 && instantaneous e_st2 && instantaneous e3
	| E_generate((_,e1),e2,e_st3,_) ->
	    instantaneous e1 && instantaneous e2 && instantaneous e_st3
	| E_vector es -> List.for_all instantaneous es
	| E_vector_mapi(is_par,(_,e1),e2,_) ->
	    not(is_par) && instantaneous e1 && instantaneous e2
	| E_int_mapi(is_par,(_,e1),e2,_) ->
	    not(is_par) && instantaneous e1 && instantaneous e2
