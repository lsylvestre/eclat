open MiniHDL_syntax

let extra_machines = ref []

let rec list_machines_s s =
  match s with
  | S_skip -> S_skip
  | S_continue q -> S_continue q
  | S_if(x,s1,so) ->
      S_if(x, list_machines_s s1,
              Option.map list_machines_s so)
  | S_case(a,hs,so) ->
      S_case(a, List.map (fun (c,s) -> c,list_machines_s s) hs,
                Option.map list_machines_s so)
  | S_letIn(x,a,s) ->
      S_letIn(x,a,list_machines_s s)
  | S_set _ -> s
  | S_acquire_lock _ | S_release_lock _
  | S_read_start _ | S_read_stop _ 
  | S_write_start _  | S_write_stop _ 
  | S_array_set _ -> s
  | S_seq(s1,s2) ->
      seq_ (list_machines_s s1)
           (list_machines_s s2)
  | S_fsm(id,rdy,result2,compute,ts,s) ->
      let sv = Ast.gensym ~prefix:"state_var" () in
      extra_machines := (id,(sv,compute,List.map fst ts)) :: !extra_machines;
      let ts,s = list_machines (ts,s) in
      S_fsm(id,rdy,result2,compute,ts,s)
  | S_in_fsm(id,s) -> S_in_fsm(id,list_machines_s s)
  | S_call _ -> s
  | S_external_run _ -> s


and list_machines (ts,s) =
 let f s = list_machines_s s in
 List.map (fun (x,s) -> x, f s) ts, f s
