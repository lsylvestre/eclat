(* Heuristic to determine if a given expression is combinational *)

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
  | External _ -> false
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
	| E_lastIn _ | E_set _ | E_par _ ->
	    false
	| E_reg _ | E_exec _ ->
	    false (* have an internal state *)
	| E_static_array_length _ ->
	    true
	| E_static_array_get _ ->
	    false
	| E_static_array_set _ ->
	    false (* side effect *)
	| E_absLabel(_,e1) -> combinational e1
	| E_appLabel(e1,_,_) -> combinational e1
	| E_for(_,e_st1,e_st2,e3,_) ->
	   combinational e_st1 && combinational e_st2 && combinational e3
	| E_generate((_,e1),e2,e_st3,_) ->
	   combinational e1 && combinational e2 && combinational e_st3
