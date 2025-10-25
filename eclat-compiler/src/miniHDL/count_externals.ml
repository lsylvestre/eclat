open MiniHDL_syntax

(** collect the names of external functions called within a MiniHDL program *)

module SMap = Map.Make(String)

let external_count : (x, unit SMap.t) Hashtbl.t = Hashtbl.create 8

let add (x:string) l =
  let s =  match Hashtbl.find_opt external_count x with 
           | None -> SMap.empty
           | Some s -> s in 
  Hashtbl.add external_count x (SMap.add l () s)

let rec count_s s =
  match s with
  | S_skip
  | S_continue _ -> ()
  | S_if(_,s1,so) ->
      count_s s1; Option.iter count_s so
  | S_case(_,hs,so) ->
      List.iter (fun (_,s) -> count_s s) hs; Option.iter count_s so
  | S_set _
  | S_read_start _
  | S_acquire_lock _ | S_release_lock _ 
  | S_read_stop _
  | S_write_start _
  | S_write_stop _
  | S_array_set _ -> ()
  | S_seq(s1,s2) -> count_s s1; count_s s2
  | S_letIn(_,_,s) ->
      count_s s
  | S_fsm(id,rdy,result,compute,ts,s) ->
      List.iter (fun (_,s) -> count_s s) ts; count_s s
  | S_in_fsm(id,s) ->
      count_s s
  | S_call _ ->
      ()
  | S_external_run(f,l,_,_,_) ->
      add f l

let count_externals_fsm (ts,s) =
  Hashtbl.clear external_count;
  List.iter (fun (_,s) -> count_s s) ts; count_s s


