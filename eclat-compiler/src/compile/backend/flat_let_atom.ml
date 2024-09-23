open Fsm_syntax

let rec flat = function
| A_letIn(x,a1,a2) ->
    let bs1,a1' = flat a1 in
    let bs2,a2' = flat a2 in
    bs1@[(x,a1')]@bs2, a2'
| A_tuple(aas) ->
    let bss,aas' = List.map flat aas |> List.split in
    List.concat bss, A_tuple(aas')
| (A_const _ | A_var _ as a) -> [],a
| A_call(op,a) ->
   let bs,a' = flat a in
   bs,A_call(op,a')
| A_string_get _
| A_buffer_get _
| A_ptr_taken _
| A_ptr_write_taken _
| A_buffer_length _
| A_encode _
| A_decode _ as a ->  (* no sub-atoms*)
    [],a
| A_vector(aas) ->
    let bss,aas' = List.map flat aas |> List.split in
    List.concat bss, A_vector(aas')

let s_let_bindings bs s =
  List.fold_right (fun (x,a) s -> S_letIn(x,a,s)) bs s

let rec flat_s s =
  match s with
  | S_skip -> S_skip
  | S_continue q ->
      S_continue q
  | S_if(x,s1,so) ->
      S_if(x, flat_s s1,Option.map flat_s so)
  | S_case(x,hs,so) ->
      S_case(x,List.map (fun (x,s) -> x,flat_s s) hs,Option.map flat_s so)
  | S_set(x,a) ->
     let bs,a' = flat a in
     s_let_bindings bs @@ S_set(x,a')
  | S_read_start(x,a) ->
      let bs,a' = flat a in
      s_let_bindings bs @@
      S_read_start(x,a')
  | S_acquire_lock _ | S_release_lock _ -> s
  | S_read_stop(x,l) ->
      s
  | S_write_start(x,a,a_upd) ->
      let bs,a' = flat a in
      let bs2,a_upd' = flat a_upd in
      s_let_bindings bs @@
      s_let_bindings bs2 @@
      S_write_start(x,a',a_upd')
  | S_write_stop _ -> s
  | S_seq(s1,s2) -> S_seq(flat_s s1,flat_s s2)
  | S_letIn(x,a,s) ->
      let bs,a' = flat a in
      s_let_bindings bs @@ S_letIn(x,a',flat_s s)
  | S_fsm(id,rdy,result,compute,ts,s) ->
      S_fsm(id,rdy,result,compute,List.map (fun (x,s) -> x,flat_s s) ts, flat_s s)
  | S_in_fsm(id,s) ->
      S_in_fsm(id,flat_s s)
  | S_call(op,a) ->
      let bs,a' = flat a in
      s_let_bindings bs @@ S_call(op,a')
  | S_external_run(f,i,res,rdy,a) ->
      let bs,a' = flat a in
      s_let_bindings bs @@S_external_run(f,i,res,rdy,a')

let flat_let_atom (ts,s) =
  List.map (fun (x,s) -> x,flat_s s) ts, flat_s s
