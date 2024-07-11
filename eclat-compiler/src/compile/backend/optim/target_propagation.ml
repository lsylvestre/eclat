open Fsm_syntax

let verbose_mode = ref false

let when_propagate x =
  if !verbose_mode then Printf.printf "Name %s has been propagated\n" x

module SMap = Ast.SMap

let propagable = function
| A_var _ | A_const _ -> true
| _ -> false

let prop_ident_a env x k =
  match SMap.find_opt x env with
  | None -> k x
  | Some a1 -> A_letIn(x,a1,k x)

let prop_ident_s env x k =
  match SMap.find_opt x env with
  | None -> k x
  | Some a1 -> S_letIn(x,a1,k x)

(* assume each let-binding has a different name *)
let rec prop env a =
  match a with
  | A_const _ -> a 
  | A_var x -> 
      (match SMap.find_opt x env with
      | None -> a
      | Some a1 -> a1)
  | A_letIn(x,a1,a2) ->
      let a1' = prop env a1 in
      if propagable a1' then
       (when_propagate x; prop (SMap.add x a1' env) a2) else A_letIn(x,a1',prop env a2)
  | A_tuple(aas) ->
      A_tuple(List.map (prop env) aas)
  | A_call(op,a1) ->
     A_call(op,prop env a1)
  | A_ptr_taken l -> 
      prop_ident_a env l (fun l' -> A_ptr_taken l')
  | A_ptr_write_taken l ->
      prop_ident_a env l (fun l' -> A_ptr_write_taken l')
  | A_string_get(x1,x2) ->
      prop_ident_a env x1 @@ fun x1' ->
      prop_ident_a env x2 @@ fun x2' -> A_string_get(x1',x2')
  | A_buffer_get x1 ->
      prop_ident_a env x1 (fun x1' -> A_buffer_get x1')
  | A_buffer_length(x1,ty) -> 
      prop_ident_a env x1 (fun x1' -> A_buffer_length(x1',ty))
  | A_encode(x,ty,n) ->
      prop_ident_a env x (fun y -> A_encode(y,ty,n))
  | A_decode(x,ty) -> 
      prop_ident_a env x (fun y -> A_decode(y,ty))
  | A_vector aas -> A_vector(List.map (prop env) aas)

let rec prop_s env s =
  match s with
  | S_skip
  | S_continue _ ->
      s
  | S_if(x,s1,so) ->
      prop_ident_s env x @@ fun x ->
        S_if(x,prop_s env s1,Option.map (prop_s env) so)
  | S_case(x,hs,so) ->
      prop_ident_s env x @@ fun x ->
      S_case(x, List.map (fun (c,s) -> c, prop_s env s) hs,Option.map (prop_s env) so)
  | S_set(x,a) ->
      (* todo: warning if x\in dom(env)) *)
      S_set(x,prop env a)
  | S_acquire_lock _ | S_release_lock _ -> s
  | S_write_start(l,a1,a2) ->
     let a1' = prop env a1 in
     let a2' = prop env a2 in
     S_write_start(l,a1',a2')
  | S_write_stop _
  | S_read_stop _ -> s
  | S_read_start(l,a1) ->
     let a1' = prop env a1 in
     S_read_start(l,a1')
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


