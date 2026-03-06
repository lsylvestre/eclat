(** This data-flow analysis computes the WAR variables occuring 
    in a given MiniHDL program.
    
    In MiniHDL implementation, each write-after-read variable
    need two synchronous signals now/next (i.e., registers) 
    to carry the updated value across clock cycles. 
    Other variables can be implemented with classical VHDL variables.

**)

open MiniHDL_syntax

module SMap = Ast.SMap

let union m1 m2 = SMap.union (fun _ _ _ -> Some ()) m1 m2
let inter m1 m2 = SMap.filter (fun x () -> SMap.mem x m2) m1
let empty = SMap.empty

let collect_a a =
  let rec collect m = function
  | A_var x -> SMap.add x () m
  | A_sig_get _ -> m
  | A_vector aa
  | A_tuple aa -> List.fold_left collect m aa
  | A_letIn(x,a1,a2) ->
      let m1 = SMap.add x () m in
      let m2 = collect m1 a1 in
      let m3 = collect m2 a2 in
      m3
  | (A_const _) -> m
  | A_call(_,a) ->
     collect m a
  | A_string_get(x,y) ->
     SMap.add x () (SMap.add y () m)
  | A_array_get(x,y) ->
     SMap.add x () (SMap.add y () m)
  | A_ptr_taken(x) -> SMap.add (MiniHDL_typing.ptr_taken x) () m
  | A_array_length(x,_) -> m
  | A_encode(x,_,_)
  | A_decode(x,_) -> (* ok ? *)
      SMap.add x () m
  | A_record bs ->
      List.fold_left collect m (List.map snd bs)
  | A_record_field(x,_,_) -> SMap.add x () m
  in
  collect empty a

let check s (r,w,war) =
  Format.(fprintf std_formatter "---- %a:\nR[" MiniHDL_syntax.Debug.pp_s s;
  SMap.iter (fun x () -> fprintf std_formatter "%s;" x) r;
  fprintf std_formatter "]\nW[";
  SMap.iter (fun x () -> fprintf std_formatter "%s;" x) w;
  fprintf std_formatter "]\nWAR[";
  SMap.iter (fun x () -> fprintf std_formatter "%s;" x) war;
    fprintf std_formatter "]\n===================\n\n"); 

  (r,w,war) ;;

(* collects read, write, war *)
let rec compute s = (* check s @@ *)
  match s with
  | S_skip
  | S_continue _ -> empty, empty, empty
  | S_if(x,s1,so) ->
      let r = SMap.singleton x () in
      let r1,w1,war1 = compute s1 in
      (match so with
      | None -> ((union r r1), w1, if SMap.mem x w1 then SMap.add x () war1 else war1)
      | Some s2 -> let r2,w2,war2 = compute s2 in
                   let r = union r (union r1 r2) in
                   let w = union w1 w2 in
                   let war = union war1 war2 in
                   r,w,if SMap.mem x w then SMap.add x () war else war)
  | S_case(x,hs,so) -> 
      let ss = List.rev_map snd hs in
      let ss = match so with Some(s2) -> s2::ss | None -> ss in
      let rec aux r1 w1 war1 = function
      | [] -> (r1,w1,war1)
      | s2::ss -> let r2,w2,war2 = compute s2 in
                  aux (union r1 r2) (union w1 w2) (union war1 war2) ss
      in
      let r,w,war = aux empty empty empty ss in
      r,w,if SMap.mem x w then SMap.add x () war else war
  | S_fsm(_,rdy,result,_,ts,s) ->
      let rec aux r1 w1 war1 = function
      | [] -> (r1,w1,war1)
      | s2::ss -> let r2,w2,war2 = compute s2 in
                  aux (union r1 r2) (union w1 w2) (union war1 war2) ss
      in
      let ss = s::List.rev_map snd ts in
      let r,w,war = aux empty empty empty ss in
      let w = SMap.add rdy () w in
      let w = SMap.add result () w in
      (r,w,war)
  | S_in_fsm(id,s1) ->
      let (r,w,war) = compute s1 in
      (r,w,war) (* ok if we move the instruction anywhere in the code ? *)
  | S_seq(s1,s2) ->
      let r1,w1,war1 = compute s1 in
      let r2,w2,war2 = compute s2 in
      union r1 r2, union w1 w2, union (union war1 war2) (inter r1 w2)
  | S_letIn(x,a,s) ->
      compute (S_seq(S_set(x,a),s))
  | S_set(x,a) ->
      let r = collect_a a in
      let w = SMap.singleton x () in
      r,w,if SMap.mem x r then (SMap.singleton x ()) else empty
  | S_sig_set(_,a) ->
      let r = collect_a a in
      (r, empty, empty)
  | S_acquire_lock(x)
  | S_release_lock(x) ->
      let m = SMap.singleton (MiniHDL_typing.ptr_taken x) () in
      (m,m,m)
  | S_read_start(_,a) ->
      let r = collect_a a in
      (r, empty, empty)
  | S_read_stop _ ->
      (empty, empty, empty)
  | S_write_start(_,a,a_upd) ->
      let r1 = collect_a a in
      let r2 = collect_a a_upd in
      (union r1 r2, empty, empty)
  | S_write_stop _ ->
      (empty, empty, empty)
  | S_array_set(_,y,a) ->
      let r = collect_a a in
      (SMap.add y () r, empty, empty)
  | S_array_from_file(_,a) ->
      let r = collect_a a in
      (r, empty, empty)
  | S_call(_,a) ->
      let r = collect_a a in
      (r, empty, empty)
  | S_external_run(_,_,_,_,a) ->
      let r = collect_a a in
      (r, empty, empty)
  | S_assert(a,_) ->
      let r = collect_a a in
      (r, empty, empty)
  | S_record_update(x,y,_,a,_) ->
      let r = collect_a a in
      (SMap.add x () @@ SMap.add y () r), empty, empty

let collect_main ~rdy ~result (ts,s) =
  let unused_id = "%" in
  let _,_,war = compute @@ S_fsm(unused_id,rdy,result,unused_id,ts,s) in
  war
  