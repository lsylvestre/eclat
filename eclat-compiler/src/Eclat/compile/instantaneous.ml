(* Heuristic to determine if a given expression is instantaneous *)

open Ast

let op_combinational ~externals (op:op) : bool =
	match op with
	| Runtime(p) ->
	    Operators.combinational ~externals p
	| GetTuple _ -> true
	| TyConstr _ -> true
	| _ -> true


let const_combinational ~externals (c:c) : bool =
	match c with
  | _ -> true

let rec combinational ?(with_sig_get=true) ~externals (e:e) : bool =
  match e with
	| E_deco(e1,loc) ->
      combinational ~externals e1
	| E_var _ ->
	    true
	| E_const c ->
	    const_combinational ~externals c
	| E_if _ | E_case _ | E_match _ ->
	    false (** lazy if/then/else is not a mux, even if subexpressions 
	              are combinational ; for instance:
	              [if false then vect_nth({0,1,2},55), 42] fails **)
	| E_app(e1,e2) ->
      (match un_deco e1 with
      | E_const(Inj _) -> combinational ~externals e2
      | E_const(Op op) ->
          op_combinational ~externals op && combinational ~externals e2
      | _ ->
         (* as we can't know locally whether [e1] is combinational,
	       we return false *)
	       false)
	| E_letIn(_,_,e1,e2) ->
	    combinational ~externals e1 && combinational ~externals e2
	| E_tuple es ->
	    List.for_all (combinational ~externals) es
	| E_fun _ | E_fix _ ->
	    false
  | E_ref _ | E_get _ | E_set _ | E_par _ ->
	    false
	| E_reg _ | E_exec _ ->
	    false (* have an internal state *)
  | E_array_length _ ->
	    true
	| E_array_make _ 
	| E_array_create _ ->
      false
	| E_array_get _
	| E_array_set _ 
	| E_array_get_start _ 
	| E_array_get_end _
	| E_array_set_immediate _ 
	| E_array_from_file _ ->
	    false (* side effect *)
	| E_for(_,_,_,e3,_) ->
	   combinational ~externals e3
	| E_generate((_,_,e1),e2,_,_,_) ->
	    combinational ~externals e1 && combinational ~externals e2
	| E_vector es -> List.for_all (combinational ~externals) es
	| E_vector_mapi(is_par,(_,_,e1),e2,_) ->
	    not(is_par) && combinational ~externals e1 && combinational ~externals e2
  | E_run _ -> false (* sometimes true, sometimes false, depending on the type *)
  | E_pause _ -> false
  | E_sig_get _ -> with_sig_get
  | E_emit _ -> false
  | E_sig_create _ -> false
  | E_loop _
  | E_trap _
  | E_exit _ 
  | E_suspend _ 
  | E_assert _ -> false
(* same as combinational, but may contain cas/match, registers or even exec blocks *)

let rec instantaneous ?(with_sig=true) ~externals (e:e) : bool =
  match e with
	| E_deco(e1,loc) ->
      instantaneous ~externals e1
	| E_var _ ->
	    true
	| E_const c ->
	    const_combinational ~externals c
	| E_if(e1,e2,e3) ->
	    instantaneous ~externals e1 && instantaneous ~externals e2 && instantaneous ~externals e3
  | E_case(e1,hs,eothers) ->
	    instantaneous ~externals e1 && List.for_all (fun (_,e) -> instantaneous ~externals e) hs && instantaneous ~externals eothers
  | E_match(e1,hs,eo) ->
      instantaneous ~externals e1 && List.for_all (fun (c,(p,e)) -> instantaneous ~externals e) hs && 
      (match eo with None -> true | Some e2 -> instantaneous ~externals e2)
	| E_app(e1,e2) ->
      (match un_deco e1 with
      | E_const(Inj _) -> instantaneous ~externals e2
      | E_const(Op op) ->
          op_combinational ~externals op && instantaneous ~externals e2
      | _ ->
         (* as we can't know locally whether [e1] is instantaneous,
	       we return false *)
	       false)
	| E_letIn(_,_,e1,e2) ->
	    instantaneous ~externals e1 && instantaneous ~externals e2
	| E_tuple es ->
	    List.for_all (instantaneous ~externals) es
	| E_fun _ | E_fix _ ->
	    false
  | E_ref _ | E_get _ | E_set _ | E_par _ ->
	    false
	| E_reg _ | E_exec _ ->
	    true (* have an internal state *)
  | E_array_length _ ->
	    true
	| E_array_make _ 
	| E_array_create _ ->
      false
	| E_array_get _
	| E_array_set _ 
	| E_array_get_start _ 
	| E_array_get_end _
	| E_array_set_immediate _
	| E_array_from_file _ ->
	    false (* side effect *)
	| E_for(_,_,_,e3,_) ->
	   instantaneous ~externals e3
	| E_generate((_,_,e1),e2,_,_,_) ->
	    instantaneous ~externals e1 && instantaneous ~externals e2
	| E_vector es -> List.for_all (instantaneous ~externals)es
	| E_vector_mapi(is_par,(_,_,e1),e2,_) ->
	    not(is_par) && instantaneous ~externals e1 && instantaneous ~externals e2
  | E_run _ -> false (* sometimes true, sometimes false, depending on the type *)
  | E_pause _ -> false
  | E_sig_get _ 
  | E_emit _ -> with_sig
  | E_sig_create _ -> true
  | E_loop _ -> false
  | E_trap _ -> true
  | E_exit _ -> false (* depends on the continuation *)
  | E_suspend(e1,_) -> instantaneous ~externals e1
  | E_assert(e1,_) -> instantaneous ~externals e1
