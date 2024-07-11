open Fsm_syntax

let verbose_mode = ref false

let when_propagate x =
  if !verbose_mode then Printf.printf "Name %s has been propagated\n" x

module SMap = Ast.SMap

let propagable = function
| A_var _ | A_const _ -> true
| _ -> false

(* assume each let-binding has a different name *)
let rec prop env a =
  match a with
  | A_const _ | A_var _ -> a
  | A_letIn(x,a1,a2) ->
      let a1' = prop env a1 in
      if propagable a1' then
       (prop (SMap.add x a1' env) a2) else A_letIn(x,a1',prop env a2)
  | A_tuple(aas) ->
      A_tuple(List.map (prop env) aas)
  | A_call(op,a1) ->
     A_call(op,prop env a1)
  | A_ptr_taken _
  | A_ptr_write_taken _
  | A_string_get _
  | A_buffer_get _
  | A_buffer_length _ -> a
  | A_encode(x,_,_) | A_decode(x,_) -> 
      (match SMap.find_opt x env with
      | None -> a
      | Some a1 -> A_letIn(x,a1,a))
  | A_vector aas -> A_vector(List.map (prop env) aas)

let rec prop_s env s =
  match s with
  | S_skip
  | S_continue _ ->
      s
  | S_if(x,s1,so) ->
      S_if(x,prop_s env s1,Option.map (prop_s env) so)
  | S_case(x,hs,so) ->
      S_case(x, List.map (fun (c,s) -> c, prop_s env s) hs,Option.map (prop_s env) so)
  | S_set(x,a) ->
      S_set(x,prop env a)
  | S_acquire_lock _ | S_release_lock _ -> s
  | S_write_start _
  | S_write_stop _
  | S_read_start _ 
  | S_read_stop _ -> s
  | S_seq(s1,s2) -> S_seq(prop_s env s1,prop_s env s2)
  | S_letIn(x,a1,s1) ->
      let a1' = prop env a1 in
      if propagable a1' then (when_propagate x; prop_s (SMap.add x a1' env) s1)
      else S_letIn(x,a1',prop_s env s1)
  | S_fsm(id,rdy,result,compute,ts,s,b) ->
      S_fsm(id,rdy,result,compute,List.map (fun (q,s) -> q, prop_s env s) ts,prop_s env s,b)
  | S_in_fsm(id,s) ->
      S_in_fsm(id,prop_s env s)
  | S_call(op,a1) ->
      S_call(op,prop env a1)

let propagation_s s =
  prop_s SMap.empty s

let propagation_fsm (ts,s) =
  (List.map (fun (q,s) -> q,propagation_s s) ts, propagation_s s)


