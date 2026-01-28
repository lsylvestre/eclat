(* ************************
open MiniHDL_syntax

(* maybe read / always write *)
let rec collect_s r w a =
  match a with
  | A_var x -> SMap.add r a
  | A_sig_get _ -> ()
  | A_tuple aa -> List.iter collect aa
  | A_letIn(x,a1,a2) ->
      add_read x;
      collect a1;
      collect a2
  | (A_const _) -> ()
  | A_call(_,a) ->
     collect a
  | A_string_get(x,y) ->
     add_read x;
     add_read y
  | A_array_get(x,y) ->
     add_read x;
     add_read y
  | A_ptr_taken(x)
  | A_array_length(x,_)
  | A_encode(x,_,_)
  | A_decode(x,_) ->
      add_read x
  | A_vector aa ->
      List.iter collect aa
  in
  collect a

(* maybe read / always write *)
let rec collect_s r w s =
  match s with
  | S_skip
  | S_continue _ -> ()
  | S_if(x,s1,so) ->
      let r = SMap.add x () r in
      let (next_r1,next_w1) = collect_s r w in
      (match so with
       | Some(s2) -> 
            let (next_r2,next_w2) = collect_s r w in
            (union next_r1 next_r2), (inter next_w1 next_w2)
       | None -> (next_r1,next_w1))
  | S_case(x,hs,so) ->
      let ss = List.rev_map snd hs in
      let ss = match so with Some(s2) -> s2::ss | None -> ss in
      let rr,ww = List.split @@ List.rev_map (collect_s r w) ss in
      let r = List.fold_right union next_r1 next_r2 in
      let w = List.fold_right inter next_w1 next_w2 in
      (r,w)
  | S_set(x,a) ->
      let r = collect_a r w a in
      let w = SMap.add x () w in
      (r, w)
  | S_sig_set(_,a) ->
      let r = collect_a r w a in
      (r,w)
  | S_acquire_lock _ | S_release_lock _ -> ()
  | S_read_start(_,a) ->
      let r = collect_a r w a in
      (r,w)
  | S_read_stop _ ->
      (r,w)
  | S_write_start(_,a,a_upd) ->
      let r = collect_a r w a in
      let r = collect_a r w a_upd in
      (r,w)
  | S_write_stop _ -> (r,w)
  | S_array_set(x,y,a) ->
      let r = SMap.add x r in
      let r = SMap.add y r in
      let r = collect_a r w a in
      (r,w)
   | S_array_from_file(y,a) ->
      let r = SMap.add y r in
      let r = collect_a r w a in
      (r,w)
  | S_seq(s1,s2) ->
      let (r,w) = collect_s r w s1 in
      let (r,w) = collect_s r w s2 in
      (r,w)
  | S_letIn(x,a,s) ->
      let r = collect_a r w a in
      let w = SMap.add x () w in
      collect_s local s
  | S_fsm(id,rdy,result,compute,ts,s) ->
      let ss = s::List.rev_map snd ts in
      let rr,ww = List.split @@ List.rev_map (collect_s r w) ss in
      let r = List.fold_right union next_r1 next_r2 in
      let w = List.fold_right inter next_w1 next_w2 in
      let w = SMap.add rdy w in
      let w = SMap.add result w in
      (r,w)
  | S_in_fsm(id,s) ->
      let (r,w) = collect_s r w s1 in
  | S_call(_,a) ->
      let r = collect_a r w a in
      (r,w)
  | S_external_run(_,_,_,_,a) ->
      let r = collect_a r w a in
      (r,w)
  | S_assert(a,_) ->
      let r = collect_a r w a in
      (r,w)





let rec collect_read_a add_read a =
  let rec collect = function
  | A_var x -> add_read x
  | A_sig_get _ -> ()
  | A_tuple aa -> List.iter collect aa
  | A_letIn(x,a1,a2) ->
      add_read x;
      collect a1;
      collect a2
  | (A_const _) -> ()
  | A_call(_,a) ->
     collect a
  | A_string_get(x,y) ->
     add_read x;
     add_read y
  | A_array_get(x,y) ->
     add_read x;
     add_read y
  | A_ptr_taken(x)
  | A_array_length(x,_)
  | A_encode(x,_,_)
  | A_decode(x,_) ->
      add_read x
  | A_vector aa ->
      List.iter collect aa
  in
  collect a


let collect vs_read vs_write ts s =
  let add_read local x = Printf.printf "--%s\n" x;
    if Hashtbl.mem local x (* || Hashtbl.mem vs_write x*) then () 
    else Hashtbl.add vs_read x ()
  in
  let rec collect_s local = function
  | S_skip -> ()
  | S_continue _ ->
      ()
  | S_if(x,s1,so) ->
      add_read local x;
      let h() = let i = Hashtbl.create 10 in
                Hashtbl.iter (fun x () -> Hashtbl.add i x ()) local;
                i
      in
      collect_s (h()) s1;
      Option.iter (collect_s (h())) so
  | S_case(x,hs,so) ->
      let h() = let i = Hashtbl.create 10 in
                Hashtbl.iter (fun x () -> Hashtbl.add i x ()) local; i in
      Hashtbl.add vs_read x ();
      List.iter (fun (_,s) -> collect_s (h()) s) hs;
      Option.iter (collect_s (h())) so
  | S_set(x,a) ->
      collect_read_a (add_read local) a; (* before writing ! *)
      Hashtbl.add local x ()
  | S_sig_set(_,a) ->
     collect_read_a (add_read local) a
  | S_acquire_lock _ | S_release_lock _ -> ()
  | S_read_start(_,a) ->
      collect_read_a (add_read local) a
  | S_read_stop _ ->
      ()
  | S_write_start(_,a,a_upd) ->
      collect_read_a (add_read local) a;
      collect_read_a (add_read local) a_upd
  | S_write_stop _ -> () 
  | S_array_set(x,y,a) ->
      Hashtbl.add vs_read x ();
      Hashtbl.add vs_read y ();
      collect_read_a (add_read local) a
   | S_array_from_file(y,a) ->
      Hashtbl.add vs_read y ();
      collect_read_a (add_read local) a
  | S_seq(s1,s2) -> collect_s local s1; collect_s local s2
  | S_letIn(x,a,s) ->
      collect_read_a (add_read local) a;
      collect_s local s
  | S_fsm(id,rdy,result,compute,ts,s) ->
      Hashtbl.add vs_read rdy ();
      Hashtbl.add vs_read result ();
      Hashtbl.add vs_read compute ();
      let ss = s::(List.map snd ts) in
      let hs = List.map (fun _ -> Hashtbl.create 5) ss in
      List.iter2 (fun h s -> collect_s h s) hs ss;
      List.iter (fun h -> Hashtbl.iter (fun x () -> Hashtbl.add local x ()) h) hs
  | S_in_fsm(id,s) ->
      collect_s local s
  | S_call(op,a) ->
      collect_read_a (add_read local) a
  | S_external_run(_,_,_,_,a) ->
      collect_read_a (add_read local) a
  | S_assert(a,_) ->
      collect_read_a (add_read local) a
  in
  let ss = s::(List.map snd ts) in
  let hs = List.map (fun _ -> Hashtbl.create 5) ss in
  List.iter2 (fun h s -> collect_s h s) hs ss;
  List.iter (fun h -> Hashtbl.iter (fun x () -> Hashtbl.add vs_write x ()) h) hs

let collect_main (ts,s) = 
  let vs_read = Hashtbl.create 10 in
  let vs_write = Hashtbl.create 10 in 
  collect vs_read vs_write ts s;
  Hashtbl.iter (fun x () -> Printf.printf "----> %s\n" x) vs_read ;;



  *************)


let collect_main (ts,s) = () ;;
  