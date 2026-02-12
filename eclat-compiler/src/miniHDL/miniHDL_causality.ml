(* 
reject:
(emit A || pause; emit A)

reject:
emit A;
emit A;

accept:
(emit A || pause; emit B);
pause

*)

open MiniHDL_syntax

module SMap = Ast.SMap


let rec can sigs s =
  match s with
  | S_skip
  | S_continue _
  | S_call _ 
  | S_external_run _-> SMap.empty
  | S_in_fsm(_,s) -> can sigs s
  | S_letIn(_,_,s) ->
      can sigs s
  | S_set _ ->  SMap.empty
  | S_sig_set(x,_) ->
      if SMap.mem x sigs then Printf.printf "============>%s\n" x;
      SMap.add x () sigs
  | S_seq(s1,s2) -> 
     let sigs2 = can sigs s1 in can sigs2 s2
  | S_fsm(id,rdy,result2,compute,ts,s) ->
     let r = ref (can sigs s) in
     List.iter (fun (_,s) -> r := Ast.(++) !r (can sigs s)) ts;
     !r
  | S_if(x,s1,None) ->
      can sigs s1
  | S_if(x,s1,Some s2) ->
      let sigs1 = can sigs s1 in
      let sigs2 = can sigs s2 in
      Ast.(++) sigs1 sigs2
  | S_case(a,hs,so) ->
      let r = ref SMap.empty in
       List.iter (fun (_,s) -> r := Ast.(++) !r (can sigs s)) hs;
      Option.iter (fun s -> r := Ast.(++) !r (can sigs s)) so;
      !r
  | S_acquire_lock _ | S_release_lock _
  | S_read_start _ | S_read_stop _ 
  | S_write_start _  | S_write_stop _ 
  | S_array_set _ 
  | S_array_from_file _ 
  | S_assert _ 
  | S_record_update _ -> SMap.empty



let check (ts,s) =
  let f = can SMap.empty in 
  ignore(f s);
  List.iter (fun (_,s) -> ignore(f s)) ts

(************ let rec check env s =
  match s with
  | S_skip -> ()
  | S_continue q -> ()
  | S_if(x,s1,so) ->
      
      S_if(x, check env s1,
              Option. (check env) so)
  (* | S_case(a,hs,so) ->
      S_case(a, List.map (fun (c,s) -> c,list_machines_s s) hs,
                Option.map list_machines_s so)
  | S_letIn(x,a,s) ->
      S_letIn(x,a,list_machines_s s)
  | S_set _ -> s
  | S_acquire_lock _ | S_release_lock _
  | S_read_start _ | S_read_stop _ 
  | S_write_start _  | S_write_stop _ 
  | S_array_set _*)
  | S_sig_set(x,a) ->

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
*************)